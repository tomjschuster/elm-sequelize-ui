module Page.Table exposing (Model, Msg, init, initialModel, update, view)

import AppUpdate exposing (AppUpdate)
import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Data.Column as Column exposing (Column, ColumnConstraints)
import Data.Combined as Combined exposing (ColumnWithConstraints, TableWithAll)
import Data.Constraints as Constraints exposing (Constraints)
import Data.DataType as DataType exposing (DataType)
import Data.Schema as Schema exposing (Schema)
import Data.Table as Table exposing (Table)
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
import Html.Attributes as Attributes exposing (checked, for, id, name, selected, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Request.Column as ColumnReq
import Request.Constraint as ConstraintReq
import Request.Schema as SchemaReq
import Request.Table as TableReq
import Router exposing (Route)
import Task
import Utils.Handlers exposing (customOnKeyDown, onChangeInt, onEnter)
import Utils.Http exposing (isUnprocessableEntity)
import Utils.Keys exposing (Key(..))
import Views.Breadcrumbs as BC
import Views.ChangesetError as CE
import Views.Column.ConstraintFields as CFields
import Views.Column.ConstraintsDisplay as ConDisplay
import Views.Column.DataTypeDisplay as DTDisplay
import Views.Column.DataTypeSelect as DTSelect


-- MODEL


type alias Model =
    { schema : Schema
    , table : Table
    , columns : List Column
    , constraints : Constraints
    , schemaTables : List Table
    , tableColumns : List Column
    , editingTable : Maybe Table
    , newColumn : Column
    , newColumnAssociation : Maybe Int
    , newColumnAssociationColumn : Maybe Int
    , editingColumn : Maybe Column
    , toDeleteId : Maybe Int
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    Model
        Schema.empty
        Table.empty
        []
        Constraints.empty
        []
        []
        Nothing
        Column.empty
        Nothing
        Nothing
        Nothing
        Nothing
        []


type alias InitialData =
    { schema : Schema
    , table : Table
    }


init : Int -> Int -> ( Model, Cmd Msg )
init schemaId tableId =
    ( { initialModel | newColumn = Column.init tableId }
    , Cmd.batch
        [ TableReq.one tableId |> Http.toTask |> Task.attempt LoadTable
        , SchemaReq.one schemaId |> Http.toTask |> Task.attempt LoadSchema
        , ColumnReq.indexForTable tableId |> Http.toTask |> Task.attempt LoadColumns
        , ConstraintReq.indexForTable tableId |> Http.toTask |> Task.attempt LoadConstraints
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | FocusResult (Result Dom.Error ())
    | Goto Route
    | LoadTableWithAll (Result Http.Error TableWithAll)
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
    | LoadSchemaTables (Result Http.Error (List Table))
    | LoadTableColumns (Result Http.Error (List Column))
      -- COLUMNS
    | LoadColumns (Result Http.Error (List Column))
      -- CREATE COLUMN
    | UpdateNewColumnConstraints ColumnConstraints
    | InputNewColumnName String
    | SelectNewColumnDataType DataType
    | CreateColumn
    | LoadNewColumn (Result Http.Error Column)
    | SetColumnAssociation (Maybe Int)
    | SetColumnAssociationColumn (Maybe Int)
    | AddNewColumnForeignKey
    | CancelColumnAssociation
      -- UPDATE COLUMN
    | EditColumn Int
    | UpdateEditingColumnConstraints ColumnConstraints
    | InputEditingColumnName String
    | SelectEditingColumnDataType DataType
    | CancelEditColumn
    | SaveEditingColumn
    | LoadEditingColumnWithConstraints (Result Http.Error ColumnWithConstraints)
      -- DELETE COLUMN
    | DestroyColumn Int
    | RemoveColumn (Result Http.Error ())
      -- CONSTRAINTS
    | LoadConstraints (Result Http.Error Constraints)


update : Msg -> Model -> ( Model, Cmd Msg, AppUpdate )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, AppUpdate.none )

        FocusResult (Ok ()) ->
            ( model, Cmd.none, AppUpdate.none )

        FocusResult (Err _) ->
            ( model, Cmd.none, AppUpdate.none )

        Goto route ->
            ( model
            , Router.goto route
            , AppUpdate.none
            )

        LoadTableWithAll (Ok { schema, table, columns, constraints }) ->
            ( { model
                | table = table
                , schema = schema
                , columns = columns
                , constraints = constraints
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadTableWithAll (Err error) ->
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
                ( model, Cmd.none, AppUpdate.httpError error )

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
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        LoadSchemaTables (Ok tables) ->
            ( { model | schemaTables = tables }, Cmd.none, AppUpdate.none )

        LoadSchemaTables (Err error) ->
            ( model, Cmd.none, AppUpdate.none )

        LoadTableColumns (Ok columns) ->
            ( { model | tableColumns = columns }, Cmd.none, AppUpdate.none )

        LoadTableColumns (Err error) ->
            ( model, Cmd.none, AppUpdate.none )

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
        UpdateNewColumnConstraints constraints ->
            ( { model | newColumn = Column.updateConstraints constraints model.newColumn }
            , Cmd.none
            , AppUpdate.none
            )

        InputNewColumnName name ->
            ( { model | newColumn = Column.updateName name model.newColumn }
            , Cmd.none
            , AppUpdate.none
            )

        SelectNewColumnDataType dataType ->
            ( { model
                | newColumn =
                    Column.updateDataType dataType model.newColumn
              }
            , Cmd.none
            , AppUpdate.none
            )

        SetColumnAssociation maybeTableId ->
            ( { model
                | newColumnAssociation = maybeTableId
                , tableColumns = []
              }
            , maybeTableId
                |> Maybe.map (ColumnReq.indexForTable >> Http.send LoadTableColumns)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        SetColumnAssociationColumn maybeColumnId ->
            ( { model
                | newColumnAssociationColumn = maybeColumnId
              }
            , Cmd.none
            , AppUpdate.none
            )

        AddNewColumnForeignKey ->
            ( { model
                | newColumnAssociation = Nothing
                , newColumnAssociationColumn = Nothing
                , tableColumns = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        CancelColumnAssociation ->
            ( { model
                | newColumnAssociation = Nothing
                , newColumnAssociationColumn = Nothing
                , tableColumns = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        CreateColumn ->
            ( model
            , ColumnReq.create model.newColumn |> Http.send LoadNewColumn
            , AppUpdate.none
            )

        LoadNewColumn (Ok column) ->
            ( { model
                | columns = model.columns ++ [ column ]
                , newColumn = Column.init model.table.id
                , newColumnAssociation = Nothing
                , newColumnAssociationColumn = Nothing
                , tableColumns = []
                , errors = []
              }
            , Cmd.batch
                [ Dom.focus "create-column" |> Task.attempt FocusResult
                , ConstraintReq.indexForTable model.table.id |> Http.send LoadConstraints
                ]
            , AppUpdate.none
            )

        LoadNewColumn (Err error) ->
            if isUnprocessableEntity error then
                ( { model | errors = ChangesetError.parseHttpError error }
                , Dom.focus "create-column" |> Task.attempt FocusResult
                , AppUpdate.none
                )
            else
                ( model, Cmd.none, AppUpdate.httpError error )

        -- EDIT COLUMN
        EditColumn id ->
            ( { model
                | editingTable = Nothing
                , editingColumn =
                    model.columns
                        |> List.filter (.id >> (==) id)
                        |> List.head
                        |> Maybe.map (Column.findAndAddConstraints model.constraints)
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
                |> Maybe.map (ColumnReq.updateWithConstraints >> Http.send LoadEditingColumnWithConstraints)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        LoadEditingColumnWithConstraints (Ok { column, constraints }) ->
            ( { model
                | constraints = constraints
                , editingColumn = Nothing
                , errors = []
              }
            , Dom.focus "create-column" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        LoadEditingColumnWithConstraints (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError (Debug.log "" error) }
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



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ breadCrumbs model.schema model.table
        , tableView model.editingTable model.table
        , createColumn model.newColumn model.newColumnAssociation model.newColumnAssociationColumn model.schemaTables model.tableColumns
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


createColumn : Column -> Maybe Int -> Maybe Int -> List Table -> List Column -> Html Msg
createColumn column newColumnAssociation newColumnAssociationColumn schemaTables tableColumns =
    form
        []
        [ fieldset []
            [ legend [] [ text "Create a column" ]
            , p
                []
                [ newColumnInput column.name ]
            , p
                []
                [ DTSelect.view
                    "create-column-data-type"
                    SelectNewColumnDataType
                    column.dataType
                ]
            , CFields.view
                "create-column-constraints"
                UpdateNewColumnConstraints
                column.constraints
            , createColumnAssociations newColumnAssociation newColumnAssociationColumn schemaTables tableColumns
            , createColumnButton
            ]
        ]


newColumnInput : String -> Html Msg
newColumnInput name =
    label []
        [ text "Name"
        , input
            [ id "create-column"
            , value name
            , onInput InputNewColumnName
            , onEnter CreateColumn
            , customOnKeyDown onColumnNameKeyDown
            ]
            []
        ]


onColumnNameKeyDown : Key -> Maybe Msg
onColumnNameKeyDown key =
    case key of
        Enter ->
            Just CreateColumn

        _ ->
            Nothing


createColumnButton : Html Msg
createColumnButton =
    button [ type_ "button", onClick CreateColumn ] [ text "Create" ]


createColumnAssociations : Maybe Int -> Maybe Int -> List Table -> List Column -> Html Msg
createColumnAssociations newColumnAssociation newColumnAssociationColumn tables columns =
    let
        buttons =
            case ( newColumnAssociation, newColumnAssociationColumn ) of
                ( Nothing, Nothing ) ->
                    []

                ( Just tableId, Nothing ) ->
                    [ button [ type_ "button", onClick CancelColumnAssociation ] [ text "Cancel" ] ]

                ( _, _ ) ->
                    [ button [ type_ "button", onClick CancelColumnAssociation ] [ text "Cancel" ]
                    , button [ type_ "button", onClick AddNewColumnForeignKey ] [ text "Add" ]
                    ]
    in
    case newColumnAssociation of
        Just tableId ->
            p []
                ([ associationTableDropdown (Just tableId) tables
                 , newColumnAssociationColumn |> createColumnAssociationsColumns columns
                 ]
                    ++ buttons
                )

        Nothing ->
            p []
                ([ associationTableDropdown Nothing tables
                 ]
                    ++ buttons
                )


createColumnAssociationsColumns : List Column -> Maybe Int -> Html Msg
createColumnAssociationsColumns columns currentColumnId =
    select
        [ onChangeInt SetColumnAssociationColumn ]
        (option [ selected (currentColumnId == Nothing) ] [ text "Select a Column" ]
            :: List.map
                (\c ->
                    option [ value (toString c.id), selected (Debug.log "a" (Just c.id == currentColumnId)) ] [ text c.name ]
                )
                columns
        )


associationTableDropdown : Maybe Int -> List Table -> Html Msg
associationTableDropdown currentTableId tables =
    select
        [ onChangeInt SetColumnAssociation ]
        (option [ selected (currentTableId == Nothing) ] [ text "Select a Table" ]
            :: List.map
                (\t ->
                    option [ value (toString t.id), selected (Just t.id == currentTableId) ] [ text t.name ]
                )
                tables
        )



-- COLUMNS VIEW


columnsView : Model -> Html Msg
columnsView model =
    section [] (columnsChildren model)


columnsChildren : Model -> List (Html Msg)
columnsChildren model =
    CE.prependIfErrors model.errors
        [ columnsTitle
        , columnList model.editingColumn model.schema.id model.columns model.constraints
        ]


columnsTitle : Html msg
columnsTitle =
    h3 [] [ text "Columns" ]


columnList : Maybe Column -> Int -> List Column -> Constraints -> Html Msg
columnList editingColumn schemaId columns constraints =
    ul [] (List.map (columnItem editingColumn schemaId constraints) columns)



-- COLUMN ITEM


columnItem : Maybe Column -> Int -> Constraints -> Column -> Html Msg
columnItem editingColumn schemaId constraints column =
    li [] (columnItemChildren editingColumn schemaId column (Column.findConstraints column.id constraints))


columnItemChildren : Maybe Column -> Int -> Column -> ColumnConstraints -> List (Html Msg)
columnItemChildren maybeEditingColumn schemaId column constraints =
    case maybeEditingColumn of
        Just editingColumn ->
            if column.id == editingColumn.id then
                [ editColumnNameInput column.name
                , DTSelect.view "edit-column-data-type" SelectEditingColumnDataType column.dataType
                , CFields.view "create-column-constraints" UpdateEditingColumnConstraints editingColumn.constraints
                , cancelEditColumnButton
                , saveEditColumnButton
                ]
            else
                [ text column.name, ConDisplay.view constraints ]

        Nothing ->
            [ columnLink schemaId column
            , DTDisplay.view column.dataType
            , editColumnButton column.id
            , deleteColumnButton column.id
            , ConDisplay.view constraints
            ]


columnLink : Int -> Column -> Html Msg
columnLink schemaId { tableId, id, name } =
    Router.link Goto (Router.Column schemaId tableId id) [] [ text name ]


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
