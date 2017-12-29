module Page.Table exposing (Model, Msg, init, initialModel, update, view)

import AppUpdate exposing (AppUpdate)
import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Data.Column as Column exposing (Column)
import Data.Column.DataType as DataType exposing (DataType)
import Data.Constraint as Constraint exposing (Constraint)
import Data.DbEntity as DbEntity exposing (DbEntity(..))
import Data.Schema as Schema exposing (Schema)
import Data.Table as Table exposing (Table)
import Data.Table.Constraints as TableConstraints exposing (TableConstraints)
import Dict exposing (Dict)
import Dom
import Html
    exposing
        ( Html
        , button
        , div
        , fieldset
        , form
        , h2
        , h3
        , input
        , label
        , legend
        , li
        , main_
        , option
        , p
        , section
        , select
        , span
        , text
        , ul
        )
import Html.Attributes as Attributes
    exposing
        ( checked
        , disabled
        , for
        , id
        , name
        , selected
        , type_
        , value
        )
import Html.Events exposing (on, onCheck, onClick, onInput)
import Http
import Request.Column as ColumnReq
import Request.Constraint as ConstraintReq
import Request.Schema as SchemaReq
import Request.Table as TableReq
import Router exposing (Route)
import Task exposing (Task)
import Utils.Handlers exposing (customOnKeyDown, onChangeId, onChangeInt, onEnter)
import Utils.Http exposing (isUnprocessableEntity)
import Utils.Keys exposing (Key(..))
import Utils.List as ListUtils
import Views.Breadcrumbs as BC
import Views.ChangesetError as CE
import Views.Column.ConstraintsDisplay as ConDisplay
import Views.Column.DataTypeDisplay as DTDisplay
import Views.Column.Edit as EditColumn


-- MODEL


type alias Model =
    { tableId : Int
    , schema : Schema
    , schemaTables : List Table
    , schemaColumns : List Column
    , tableConstraints : List Constraint
    , editingTable : Maybe Table
    , newColumn : Column
    , editingColumn : Maybe Column
    , toDeleteColumnId : Maybe Int
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    { tableId = 0
    , schema = Schema.empty
    , schemaTables = []
    , schemaColumns = []
    , tableConstraints = []
    , editingTable = Nothing
    , newColumn = Column.empty
    , editingColumn = Nothing
    , toDeleteColumnId = Nothing
    , errors = []
    }


init : Int -> Int -> ( Model, Cmd Msg )
init schemaId tableId =
    ( { initialModel | newColumn = Column.init tableId, tableId = tableId }
    , Task.sequence
        [ SchemaReq.one schemaId |> sendDbEntity DbSchema
        , TableReq.indexForSchema schemaId |> sendDbEntity DbTables
        , ColumnReq.indexForSchema schemaId |> sendDbEntity DbColumns
        , ConstraintReq.indexForTable tableId |> sendDbEntity DbConstraints
        ]
        |> Task.attempt LoadDbEntities
    )


sendDbEntity : (a -> DbEntity) -> Http.Request a -> Task Http.Error DbEntity
sendDbEntity toEntity =
    Http.toTask >> Task.map toEntity



-- UPDATE


type Msg
    = NoOp
    | FocusResult (Result Dom.Error ())
    | Goto Route
    | LoadDbEntity (Result Http.Error DbEntity)
    | LoadDbEntities (Result Http.Error (List DbEntity))
      -- TABLE
    | EditTable
    | InputTableName String
    | CancelEditTable
    | SaveTableName
    | Destroy
    | RemoveTable (Result Http.Error ())
      -- COLUMNS
      -- CREATE COLUMN
    | UpdateNewColumn Column
    | CreateColumn
      -- UPDATE COLUMN
    | EditColumn Int
    | UpdateEditingColumn Column
    | CancelEditColumn
    | SaveEditingColumn
      -- DELETE COLUMN
    | DestroyColumn Int
    | RemoveColumn (Result Http.Error ())


handleHttpError : Model -> Http.Error -> ( Model, Cmd Msg, AppUpdate )
handleHttpError model error =
    if isUnprocessableEntity error then
        ( { model | errors = ChangesetError.parseHttpError error }
        , Cmd.none
        , AppUpdate.none
        )
    else
        ( model, Cmd.none, AppUpdate.httpError error )


update : Msg -> Model -> ( Model, Cmd Msg, AppUpdate )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, AppUpdate.none )

        FocusResult _ ->
            ( model, Cmd.none, AppUpdate.none )

        Goto route ->
            ( model
            , Router.goto route
            , AppUpdate.none
            )

        LoadDbEntity (Ok entity) ->
            ( updateWithDbEntity entity { model | errors = [] }
            , Cmd.none
            , AppUpdate.none
            )

        LoadDbEntity (Err error) ->
            handleHttpError model error

        LoadDbEntities (Ok entities) ->
            ( updateWithDbEntities entities { model | errors = [] }
            , Dom.focus "create-column" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        LoadDbEntities (Err error) ->
            handleHttpError model error

        -- TABLE
        EditTable ->
            ( { model
                | editingTable =
                    model.schemaTables
                        |> List.filter (.id >> (==) model.tableId)
                        |> List.head
                , editingColumn = Nothing
                , errors = []
              }
            , Dom.focus "edit-table-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        InputTableName name ->
            ( { model
                | editingTable =
                    Maybe.map (Table.updateName name) model.editingTable
              }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditTable ->
            ( { model | editingTable = Nothing }
            , Cmd.none
            , AppUpdate.none
            )

        SaveTableName ->
            ( model
            , model.editingTable
                |> Maybe.map (TableReq.update >> sendDbEntity DbUpdatedTable >> Task.attempt LoadDbEntity)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        Destroy ->
            ( model
            , TableReq.destroy model.tableId |> Http.send RemoveTable
            , AppUpdate.none
            )

        RemoveTable (Ok ()) ->
            ( { model | errors = [] }
            , Router.goto (Router.Schema model.schema.id)
            , AppUpdate.none
            )

        RemoveTable (Err error) ->
            handleHttpError model error

        -- COLUMNS
        -- NEW COLUMN
        UpdateNewColumn newColumn ->
            ( { model | newColumn = newColumn }
            , Cmd.none
            , AppUpdate.none
            )

        CreateColumn ->
            ( model
            , ColumnReq.create model.newColumn
                |> Http.toTask
                |> Task.andThen
                    (\column ->
                        Task.sequence
                            [ Task.succeed (DbUpdatedColumn column)
                            , ConstraintReq.indexForTable model.tableId |> sendDbEntity DbConstraints
                            ]
                    )
                |> Task.attempt LoadDbEntities
            , AppUpdate.none
            )

        -- EDIT COLUMN
        EditColumn id ->
            let
                editingColumn =
                    model.schemaColumns
                        |> List.filter (.id >> (==) id)
                        |> List.head
                        |> Maybe.map
                            (Column.findAndAddConstraints
                                Dict.empty
                                -- tableReferences
                                Dict.empty
                                -- Column References
                                (TableConstraints.fromList model.tableConstraints)
                            )
            in
            ( { model
                | editingTable = Nothing
                , editingColumn =
                    model.schemaColumns
                        |> List.filter (.id >> (==) id)
                        |> List.head
                        |> Maybe.map
                            (Column.findAndAddEditingConstraints
                                Dict.empty
                                -- Column References
                                (TableConstraints.fromList model.tableConstraints)
                            )
                , errors = []
              }
            , Dom.focus "edit-column-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        UpdateEditingColumn editingColumn ->
            let
                sameDataType =
                    model.editingColumn
                        |> Maybe.map (.dataType >> (/=) editingColumn.dataType)
                        |> Maybe.withDefault False

                newDataType =
                    sameDataType && editingColumn.dataType /= DataType.none
            in
            ( { model | editingColumn = Just editingColumn }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditColumn ->
            ( { model
                | editingColumn = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        SaveEditingColumn ->
            ( model
            , model.editingColumn
                |> Maybe.map (ColumnReq.updateWithConstraints >> sendDbEntity DbUpdatedColumn >> Task.attempt LoadDbEntity)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        DestroyColumn id ->
            ( { model | toDeleteColumnId = Just id }
            , ColumnReq.destroy id |> Http.send RemoveColumn
            , AppUpdate.none
            )

        RemoveColumn (Ok ()) ->
            ( { model
                | errors = []
                , schemaColumns =
                    model.toDeleteColumnId
                        |> Maybe.map (Column.removeFromList model.schemaColumns)
                        |> Maybe.withDefault model.schemaColumns
              }
            , Cmd.none
            , AppUpdate.none
            )

        RemoveColumn (Err error) ->
            if isUnprocessableEntity error then
                ( { model | errors = ChangesetError.parseHttpError error }
                , Dom.focus "create-column" |> Task.attempt FocusResult
                , AppUpdate.none
                )
            else
                ( model, Cmd.none, AppUpdate.httpError error )


updateWithDbEntities : List DbEntity -> Model -> Model
updateWithDbEntities entities model =
    List.foldl updateWithDbEntity model entities


updateWithDbEntity : DbEntity -> Model -> Model
updateWithDbEntity entity model =
    case entity of
        DbSchema schema ->
            { model | schema = schema }

        DbTables tables ->
            { model | schemaTables = tables }

        DbUpdatedTable table ->
            { model
                | schemaTables =
                    ListUtils.replaceIfMatch .id table model.schemaTables
                , editingTable = Nothing
            }

        DbColumns columns ->
            { model | schemaColumns = columns }

        DbNewColumn column ->
            { model
                | schemaColumns = ListUtils.add column model.schemaColumns
                , newColumn = Column.init model.tableId
                , schemaTables = []
            }

        DbUpdatedColumn column ->
            { model
                | schemaColumns =
                    ListUtils.replaceIfMatch .id column model.schemaColumns
                , editingColumn = Nothing
            }

        DbConstraints tableConstraints ->
            { model | tableConstraints = tableConstraints }

        _ ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    let
        table =
            model.schemaTables
                |> List.filter (.id >> (==) model.tableId)
                |> List.head
                |> Maybe.withDefault Table.empty

        columns =
            model.schemaColumns
                |> List.filter (.tableId >> (==) model.tableId)
    in
    case model.errors of
        [] ->
            main_ []
                [ breadCrumbs model.schema table
                , tableView model.editingTable table
                , newColumnView
                    model.newColumn
                    model.schemaTables
                    model.schemaColumns
                , columnsView model
                ]

        errors ->
            main_ []
                [ breadCrumbs model.schema table
                , tableView model.editingTable table
                , CE.view model.errors
                , newColumnView
                    model.newColumn
                    model.schemaTables
                    model.schemaColumns
                , columnsView model
                ]


breadCrumbs : Schema -> Table -> Html Msg
breadCrumbs schema table =
    BC.view Goto [ BC.home, BC.schema schema, BC.table table ]



-- TABLE VIEW


tableView : Maybe Table -> Table -> Html Msg
tableView editingTable table =
    section [] (tableChildren editingTable table)


tableChildren : Maybe Table -> Table -> List (Html Msg)
tableChildren editingTable table =
    editingTable
        |> Maybe.map editingTableChildren
        |> Maybe.withDefault (readTableChildren table)



-- READ TABLE


readTableChildren : Table -> List (Html Msg)
readTableChildren { name } =
    [ tableName name
    , editTableNameButton
    , deleteTableButton
    ]


tableName : String -> Html Msg
tableName name =
    h2 [] [ text name ]


editTableNameButton : Html Msg
editTableNameButton =
    button [ onClick EditTable ] [ text "Edit Name" ]


deleteTableButton : Html Msg
deleteTableButton =
    button [ onClick Destroy ] [ text "Delete Table" ]



-- UPDATE TABLE


editingTableChildren : Table -> List (Html Msg)
editingTableChildren { name } =
    [ editTableNameInput name
    , cancelEditTableButton
    , saveEditTableButton
    ]


editTableNameInput : String -> Html Msg
editTableNameInput name =
    input
        [ id "edit-table-name"
        , value name
        , onInput InputTableName
        , customOnKeyDown onTableNameKeyDown
        ]
        []


onTableNameKeyDown : Key -> Maybe Msg
onTableNameKeyDown key =
    case key of
        Enter ->
            Just SaveTableName

        Escape ->
            Just CancelEditTable

        _ ->
            Nothing


cancelEditTableButton : Html Msg
cancelEditTableButton =
    button [ onClick CancelEditTable ] [ text "Cancel" ]


saveEditTableButton : Html Msg
saveEditTableButton =
    button [ onClick SaveTableName ] [ text "Save" ]



-- NEW COLUMN


newColumnView : Column -> List Table -> List Column -> Html Msg
newColumnView column tables columns =
    section []
        [ newColumnTitle
        , EditColumn.view
            CreateColumn
            UpdateNewColumn
            "Create"
            column
            tables
            columns
        ]


newColumnTitle : Html msg
newColumnTitle =
    h3 [] [ text "New Column" ]



-- COLUMNS VIEW


columnsView : Model -> Html Msg
columnsView model =
    let
        tableConstraints =
            TableConstraints.fromList model.tableConstraints
    in
    section []
        [ columnsTitle
        , columnList model tableConstraints
        ]


columnsTitle : Html msg
columnsTitle =
    h3 [] [ text "Columns" ]


columnList : Model -> TableConstraints -> Html Msg
columnList model tableConstraints =
    let
        columns =
            model.schemaColumns
                |> List.filter (.tableId >> (==) model.tableId)
    in
    ul [] (List.map (columnItem model tableConstraints) columns)



-- COLUMN ITEM


columnItem : Model -> TableConstraints -> Column -> Html Msg
columnItem model tableConstraints column =
    let
        columnConstraints =
            Column.buildConstraints
                Dict.empty
                -- tableReferences
                Dict.empty
                -- Column References
                column.id
                tableConstraints
    in
    case model.editingColumn of
        Just editingColumn ->
            if editingColumn.id == column.id then
                li []
                    [ EditColumn.view
                        SaveEditingColumn
                        UpdateEditingColumn
                        "Save"
                        editingColumn
                        []
                        -- Editing Column Table References
                        []

                    -- Editing Column Column References
                    , cancelEditColumnButton
                    ]
            else
                li []
                    [ p [] [ text column.name ]
                    , DTDisplay.view column.dataType
                    , editColumnButton column.id
                    , deleteColumnButton column.id
                    , ConDisplay.view columnConstraints
                    ]

        Nothing ->
            li []
                [ p [] [ text column.name ]
                , DTDisplay.view column.dataType
                , editColumnButton column.id
                , deleteColumnButton column.id
                , ConDisplay.view columnConstraints
                ]


editColumnButton : Int -> Html Msg
editColumnButton id =
    button [ onClick (EditColumn id) ] [ text "Edit" ]


deleteColumnButton : Int -> Html Msg
deleteColumnButton id =
    button [ onClick (DestroyColumn id) ] [ text "Delete" ]


cancelEditColumnButton : Html Msg
cancelEditColumnButton =
    button [ onClick CancelEditColumn ] [ text "Cancel" ]


saveEditColumnButton : Html Msg
saveEditColumnButton =
    button [ onClick SaveEditingColumn ] [ text "Save" ]
