module Page.Table exposing (Model, Msg, init, initialModel, update, view)

import AppUpdate exposing (AppUpdate)
import Array exposing (Array)
import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Data.Column as Column exposing (Column)
import Data.Column.Constraints as ColumnConstraints exposing (ColumnConstraints)
import Data.Column.DataType as DataType exposing (DataType)
import Data.Constraint as Constraint exposing (Constraint)
import Data.DbEntity as DbEntity exposing (DbEntity(..))
import Data.Schema as Schema exposing (Schema)
import Data.Table as Table exposing (Table, TableConstraints)
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
import Views.Breadcrumbs as BC
import Views.ChangesetError as CE
import Views.Column.ConstraintFields as CFields
import Views.Column.ConstraintsDisplay as ConDisplay
import Views.Column.DataTypeDisplay as DTDisplay
import Views.Column.DataTypeSelect as DTSelect
import Views.Column.Edit as EditColumn


-- MODEL


type alias Model =
    { schema : Schema
    , table : Table
    , columns : List Column
    , constraints : List Constraint
    , tableReferences : Dict Int Table
    , columnReferences : Dict Int Column
    , editingTable : Maybe Table
    , newColumn : Column
    , newColumnReferenceTables : List Table
    , newColumnReferenceColumns : List Column
    , editingColumn : Maybe Column
    , toDeleteId : Maybe Int
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    { schema = Schema.empty
    , table = Table.empty
    , columns = []
    , constraints = []
    , tableReferences = Dict.empty
    , columnReferences = Dict.empty
    , editingTable = Nothing
    , newColumn = Column.empty
    , newColumnReferenceTables = []
    , newColumnReferenceColumns = []
    , editingColumn = Nothing
    , toDeleteId = Nothing
    , errors = []
    }


type alias InitialData =
    { schema : Schema
    , table : Table
    }


type NewColumnAssoc
    = SelectTable
    | SelectColumn Int (List Column)
    | NewColumnAssocReady Int (List Column) Int


newColumnAssocsToIds : Array NewColumnAssoc -> List Int
newColumnAssocsToIds =
    Array.toList >> List.filterMap newColumnAssocToId


newColumnAssocToId : NewColumnAssoc -> Maybe Int
newColumnAssocToId assoc =
    case assoc of
        NewColumnAssocReady _ _ columnId ->
            Just columnId

        _ ->
            Nothing


init : Int -> Int -> ( Model, Cmd Msg )
init schemaId tableId =
    ( { initialModel | newColumn = Column.init tableId }
    , Task.sequence
        [ TableReq.one tableId |> sendDbEntity DbTable
        , SchemaReq.one schemaId |> sendDbEntity DbSchema
        , ColumnReq.indexForTable tableId |> sendDbEntity DbColumns
        , TableReq.indexReferences tableId |> sendDbEntity DbReferenceTables
        , ColumnReq.indexReferences tableId |> sendDbEntity DbReferenceColumns
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
      -- SCHEMA
    | LoadSchema (Result Http.Error Schema)
      -- TABLE
    | LoadTable (Result Http.Error Table)
    | EditTable
    | InputTableName String
    | CancelEditTable
    | SaveTableName
    | Destroy
    | RemoveTable (Result Http.Error ())
      -- COLUMNS
    | LoadColumns (Result Http.Error (List Column))
      -- CREATE COLUMN
    | UpdateNewColumn Column
    | LoadNewColumnReferenceCandidates (Result Http.Error ( List Table, List Column ))
    | CreateColumn
      -- UPDATE COLUMN
    | EditColumn Int
    | UpdateEditingColumnConstraints ColumnConstraints
    | InputEditingColumnName String
    | SelectEditingColumnDataType DataType
    | CancelEditColumn
    | SaveEditingColumn
    | LoadEditingColumn (Result Http.Error Column)
      -- DELETE COLUMN
    | DestroyColumn Int
    | RemoveColumn (Result Http.Error ())
      -- CONSTRAINTS
    | LoadConstraints (Result Http.Error (List Constraint))


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
            if isUnprocessableEntity error then
                ( { model | errors = ChangesetError.parseHttpError error }
                , Cmd.none
                , AppUpdate.none
                )
            else
                ( model, Cmd.none, AppUpdate.httpError error )

        LoadDbEntities (Ok entities) ->
            ( updateWithDbEntities entities { model | errors = [], newColumnReferenceTables = [] }
            , Dom.focus "create-column" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        LoadDbEntities (Err error) ->
            if isUnprocessableEntity error then
                ( { model | errors = ChangesetError.parseHttpError error }
                , Cmd.none
                , AppUpdate.none
                )
            else
                ( model, Cmd.none, AppUpdate.httpError error )

        -- SCHEMA
        LoadSchema (Ok schema) ->
            ( { model
                | schema = schema
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadSchema (Err error) ->
            if isUnprocessableEntity error then
                ( { model | errors = ChangesetError.parseHttpError error }
                , Cmd.none
                , AppUpdate.none
                )
            else
                ( model, Cmd.none, AppUpdate.httpError error )

        -- TABLE
        LoadTable (Ok table) ->
            ( { model
                | table = table
                , editingTable = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadTable (Err error) ->
            if isUnprocessableEntity error then
                ( { model | errors = ChangesetError.parseHttpError error }
                , Cmd.none
                , AppUpdate.none
                )
            else
                handleHttpError model error

        EditTable ->
            ( { model
                | editingTable = Just model.table
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
                |> Maybe.map (TableReq.update >> Http.send LoadTable)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        Destroy ->
            ( model
            , TableReq.destroy model.table.id |> Http.send RemoveTable
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
        LoadColumns (Ok columns) ->
            ( { model
                | columns = columns
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadColumns (Err error) ->
            if isUnprocessableEntity error then
                ( { model | errors = ChangesetError.parseHttpError error }
                , Cmd.none
                , AppUpdate.none
                )
            else
                ( model, Cmd.none, AppUpdate.httpError error )

        -- NEW COLUMN
        UpdateNewColumn newColumn ->
            let
                newDataType =
                    newColumn.dataType /= model.newColumn.dataType && newColumn.dataType /= DataType.none

                cmd =
                    if newDataType then
                        TableReq.indexReferenceCandidates model.schema.id newColumn.dataType
                            |> Http.send LoadNewColumnReferenceCandidates
                    else
                        Cmd.none
            in
            ( { model | newColumn = newColumn }
            , cmd
            , AppUpdate.none
            )

        LoadNewColumnReferenceCandidates (Ok ( tables, columns )) ->
            ( { model | newColumnReferenceTables = tables, newColumnReferenceColumns = columns }
            , Cmd.none
            , AppUpdate.none
            )

        LoadNewColumnReferenceCandidates (Err error) ->
            handleHttpError model error

        CreateColumn ->
            ( model
            , Task.sequence
                [ ColumnReq.create model.newColumn |> sendDbEntity DbNewColumn
                , ConstraintReq.indexForTable model.table.id |> sendDbEntity DbConstraints
                , TableReq.indexReferences model.table.id |> sendDbEntity DbReferenceTables
                , ColumnReq.indexReferences model.table.id |> sendDbEntity DbReferenceColumns
                ]
                |> Task.attempt LoadDbEntities
            , AppUpdate.none
            )

        -- EDIT COLUMN
        EditColumn id ->
            ( { model
                | editingTable = Nothing
                , editingColumn =
                    model.columns
                        |> List.filter (.id >> (==) id)
                        |> List.head
                        |> Maybe.map
                            (Column.findAndAddConstraints
                                model.tableReferences
                                model.columnReferences
                                (Table.buildConstraints model.constraints)
                            )
                , errors = []
              }
            , Dom.focus "edit-column-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        UpdateEditingColumnConstraints constraints ->
            ( { model | editingColumn = Maybe.map (Column.updateConstraints constraints) model.editingColumn }
            , Cmd.none
            , AppUpdate.none
            )

        InputEditingColumnName name ->
            ( { model
                | editingColumn =
                    Maybe.map (Column.updateName name) model.editingColumn
              }
            , Cmd.none
            , AppUpdate.none
            )

        SelectEditingColumnDataType dataType ->
            ( { model
                | editingColumn =
                    Maybe.map (Column.updateDataType dataType) model.editingColumn
              }
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
                |> Maybe.map (ColumnReq.updateWithConstraints >> Http.send LoadEditingColumn)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        LoadEditingColumn (Ok column) ->
            ( { model
                | columns = replaceOrAppendColumn column model.columns
                , editingColumn = Nothing
                , errors = []
              }
            , Cmd.batch
                [ Dom.focus "create-column" |> Task.attempt FocusResult
                , ConstraintReq.indexForTable model.table.id |> Http.send LoadConstraints
                ]
            , AppUpdate.none
            )

        LoadEditingColumn (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Dom.focus "edit-column-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        DestroyColumn id ->
            ( { model | toDeleteId = Just id }
            , ColumnReq.destroy id |> Http.send RemoveColumn
            , AppUpdate.none
            )

        RemoveColumn (Ok ()) ->
            ( { model
                | errors = []
                , columns =
                    model.toDeleteId
                        |> Maybe.map (Column.removeFromList model.columns)
                        |> Maybe.withDefault model.columns
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

        -- CONSTRAINTS
        LoadConstraints (Ok constraints) ->
            ( { model
                | constraints = constraints
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadConstraints (Err error) ->
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

        DbTable table ->
            { model | table = table, editingTable = Nothing }

        DbReferenceTables tables ->
            { model
                | tableReferences =
                    List.foldl (\c -> Dict.insert c.id c) model.tableReferences tables
            }

        DbDataTypeTables tables ->
            { model | newColumnReferenceTables = tables }

        DbNewColumn column ->
            { model
                | columns = model.columns ++ [ column ]
                , newColumn = Column.init model.table.id
                , newColumnReferenceTables = []
            }

        DbUpdatedColumn column ->
            { model
                | columns = List.map (Column.replaceIfMatch column) model.columns
                , editingColumn = Nothing
            }

        DbColumns columns ->
            { model | columns = columns }

        DbReferenceColumns columns ->
            { model
                | columnReferences =
                    List.foldl (\c -> Dict.insert c.id c) model.columnReferences columns
            }

        DbConstraints constraints ->
            { model | constraints = constraints }

        _ ->
            model


replaceOrAppendColumn : Column -> List Column -> List Column
replaceOrAppendColumn column columns =
    let
        ( replaced, newColumns ) =
            List.foldr
                (\x ( replaced, xs ) ->
                    if x.id == column.id then
                        ( True, column :: xs )
                    else
                        ( replaced, x :: xs )
                )
                ( False, [] )
                columns
    in
    if replaced then
        newColumns
    else
        newColumns ++ [ column ]



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ breadCrumbs model.schema model.table
        , tableView model.editingTable model.table
        , createColumn model.newColumn model.newColumnReferenceTables model.newColumnReferenceColumns
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



-- CREATE COLUMN


createColumn : Column -> List Table -> List Column -> Html Msg
createColumn column tables columns =
    section []
        [ EditColumn.view
            CreateColumn
            UpdateNewColumn
            "Create"
            column
            tables
            columns
        ]



-- COLUMNS VIEW


columnsView : Model -> Html Msg
columnsView model =
    section [] (columnsChildren model)


columnsChildren : Model -> List (Html Msg)
columnsChildren model =
    let
        tableConstraints =
            Table.buildConstraints model.constraints
    in
    CE.prependIfErrors model.errors
        [ columnsTitle
        , columnList model tableConstraints
        ]


columnsTitle : Html msg
columnsTitle =
    h3 [] [ text "Columns" ]


columnList : Model -> TableConstraints -> Html Msg
columnList model tableConstraints =
    ul [] (List.map (columnItem model tableConstraints) model.columns)



-- COLUMN ITEM


columnItem : Model -> TableConstraints -> Column -> Html Msg
columnItem model tableConstraints column =
    li [] (columnItemChildren model tableConstraints column)


columnItemChildren : Model -> TableConstraints -> Column -> List (Html Msg)
columnItemChildren model tableConstraints column =
    let
        columnConstraints =
            Column.buildConstraints
                model.tableReferences
                model.columnReferences
                column.id
                tableConstraints
    in
    case model.editingColumn of
        Just editingColumn ->
            if column.id == editingColumn.id then
                [ editColumnNameInput editingColumn.name
                , DTSelect.view "edit-column-data-type" SelectEditingColumnDataType column.dataType
                , CFields.view "create-column-constraints" UpdateEditingColumnConstraints editingColumn.constraints
                , cancelEditColumnButton
                , saveEditColumnButton
                ]
            else
                [ text column.name
                , ConDisplay.view columnConstraints
                ]

        Nothing ->
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


editColumnNameInput : String -> Html Msg
editColumnNameInput name =
    input
        [ id "edit-column-name"
        , value name
        , onInput InputEditingColumnName
        , customOnKeyDown onEditingColumnNameKeyDown
        ]
        []


onEditingColumnNameKeyDown : Key -> Maybe Msg
onEditingColumnNameKeyDown key =
    case key of
        Enter ->
            Just SaveEditingColumn

        Escape ->
            Just CancelEditColumn

        _ ->
            Nothing


cancelEditColumnButton : Html Msg
cancelEditColumnButton =
    button [ onClick CancelEditColumn ] [ text "Cancel" ]


saveEditColumnButton : Html Msg
saveEditColumnButton =
    button [ onClick SaveEditingColumn ] [ text "Save" ]
