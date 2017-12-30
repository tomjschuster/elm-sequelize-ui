module Page.Table exposing (Model, Msg, init, initialModel, update, view)

import AppUpdate exposing (AppUpdate)
import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Data.Column as Column exposing (Column)
import Data.Constraint exposing (Constraint)
import Data.DbEntity exposing (DbEntity(..))
import Data.Schema as Schema exposing (Schema)
import Data.Table as Table exposing (Table)
import Data.Table.Constraints as TableConstraints exposing (TableConstraints)
import Dom
import Html
    exposing
        ( Html
        , button
        , h2
        , h3
        , input
        , li
        , main_
        , p
        , section
        , text
        , ul
        )
import Html.Attributes as Attr
import Html.Events as Evt
import Http
import Request.Column as ColumnReq
import Request.Constraint as ConstraintReq
import Request.Schema as SchemaReq
import Request.Table as TableReq
import Router exposing (Route)
import Task exposing (Task)
import Utils.Events as EvtUtils
import Utils.Http as HttpUtils
import Utils.Keys exposing (Key(..))
import Utils.List as ListUtils
import Views.Breadcrumbs as BreadCrumbs
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


handleHttpError : Model -> Http.Error -> ( Model, Cmd Msg, AppUpdate )
handleHttpError model error =
    if HttpUtils.isUnprocessableEntity error then
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
                |> Maybe.map
                    (TableReq.update
                        >> sendDbEntity DbUpdatedTable
                        >> Task.attempt LoadDbEntity
                    )
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
                            [ Task.succeed (DbNewColumn column)
                            , ConstraintReq.indexForTable model.tableId
                                |> sendDbEntity DbConstraints
                            ]
                    )
                |> Task.attempt LoadDbEntities
            , AppUpdate.none
            )

        -- EDIT COLUMN
        EditColumn id ->
            ( { model
                | editingTable = Nothing
                , editingColumn =
                    model.schemaColumns
                        |> ListUtils.find (.id >> (==) id)
                        |> Maybe.map
                            (Column.findAndAddEditingConstraints
                                (ListUtils.toLookup .id model.schemaColumns)
                                (TableConstraints.fromList model.tableConstraints)
                            )
                , errors = []
              }
            , Dom.focus "edit-column-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        UpdateEditingColumn editingColumn ->
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
                |> Maybe.map
                    (ColumnReq.updateWithConstraints
                        >> Http.toTask
                        >> Task.andThen
                            (\column ->
                                Task.sequence
                                    [ Task.succeed (DbUpdatedColumn column)
                                    , ConstraintReq.indexForTable model.tableId
                                        |> sendDbEntity DbConstraints
                                    ]
                            )
                        >> Task.attempt LoadDbEntities
                    )
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        DestroyColumn id ->
            ( model
            , ColumnReq.destroy id
                |> Http.toTask
                |> Task.map (always (DbDeletedColumn id))
                |> Task.attempt LoadDbEntity
            , AppUpdate.none
            )


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
            }

        DbUpdatedColumn column ->
            { model
                | schemaColumns =
                    ListUtils.replaceIfMatch .id column model.schemaColumns
                , editingColumn = Nothing
            }

        DbDeletedColumn id ->
            { model | schemaColumns = List.filter (.id >> (/=) id) model.schemaColumns }

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
                |> ListUtils.findWithDefault
                    Table.empty
                    (.id >> (==) model.tableId)
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
                , CE.view errors
                , newColumnView
                    model.newColumn
                    model.schemaTables
                    model.schemaColumns
                , columnsView model
                ]


breadCrumbs : Schema -> Table -> Html Msg
breadCrumbs schema table =
    BreadCrumbs.view Goto
        [ BreadCrumbs.home, BreadCrumbs.schema schema, BreadCrumbs.table table ]



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
    button [ Evt.onClick EditTable ] [ text "Edit Name" ]


deleteTableButton : Html Msg
deleteTableButton =
    button [ Evt.onClick Destroy ] [ text "Delete Table" ]



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
        [ Attr.id "edit-table-name"
        , Attr.value name
        , Evt.onInput InputTableName
        , EvtUtils.customOnKeyDown onTableNameKeyDown
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
    button [ Evt.onClick CancelEditTable ] [ text "Cancel" ]


saveEditTableButton : Html Msg
saveEditTableButton =
    button [ Evt.onClick SaveTableName ] [ text "Save" ]



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
                (ListUtils.toLookup .id model.schemaTables)
                (ListUtils.toLookup .id model.schemaColumns)
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
                        model.schemaTables
                        model.schemaColumns
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
    button [ Evt.onClick (EditColumn id) ] [ text "Edit" ]


deleteColumnButton : Int -> Html Msg
deleteColumnButton id =
    button [ Evt.onClick (DestroyColumn id) ] [ text "Delete" ]


cancelEditColumnButton : Html Msg
cancelEditColumnButton =
    button [ Evt.onClick CancelEditColumn ] [ text "Cancel" ]
