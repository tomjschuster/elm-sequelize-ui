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
import Html.Attributes exposing (checked, for, id, name, selected, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Request.Column as RC
import Request.Table as RT
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
    , editingTable : Maybe Table
    , newColumn : Column
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
        Nothing
        Column.empty
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
    , RT.oneWithAll tableId |> Http.toTask |> Task.attempt LoadTableWithAll
    )



-- UPDATE


type Msg
    = NoOp
    | FocusResult (Result Dom.Error ())
    | Goto Route
    | LoadTableWithAll (Result Http.Error TableWithAll)
      -- TABLE
    | LoadTable (Result Http.Error Table)
    | EditTable
    | InputTableName String
    | CancelEditTable
    | SaveTableName
    | Destroy
    | RemoveTable (Result Http.Error ())
      -- COLUMNS
      -- CREATE COLUMN
    | UpdateNewColumnConstraints ColumnConstraints
    | InputNewColumnName String
    | SelectNewColumnDataType DataType
    | CreateColumn
    | LoadNewColumnWithConstraints (Result Http.Error ColumnWithConstraints)
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
                |> Maybe.map (RT.update >> Http.send LoadTable)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        Destroy ->
            ( model
            , RT.destroy model.table.id |> Http.send RemoveTable
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

        -- COLUMNS
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

        CreateColumn ->
            ( model
            , RC.create model.newColumn
                |> Http.send LoadNewColumnWithConstraints
            , AppUpdate.none
            )

        LoadNewColumnWithConstraints (Ok { column, constraints }) ->
            ( { model
                | columns = model.columns ++ [ column ]
                , constraints = constraints
                , newColumn = Column.init model.table.id
                , errors = []
              }
            , Dom.focus "create-column" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        LoadNewColumnWithConstraints (Err error) ->
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
                |> Maybe.map (RC.updateWithConstraints >> Http.send LoadEditingColumnWithConstraints)
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
            , RC.destroy id |> Http.send RemoveColumn
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
            ( { model
                | errors = ChangesetError.parseHttpError error
                , toDeleteId = Nothing
              }
            , Cmd.none
            , AppUpdate.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ breadCrumbs model.schema model.table
        , tableView model.editingTable model.table
        , createColumn model.newColumn
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


createColumn : Column -> Html Msg
createColumn column =
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
            ]
            []
        ]


createColumnButton : Html Msg
createColumnButton =
    button [ type_ "button", onClick CreateColumn ] [ text "Create" ]



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
        , customOnKeyDown onColumnNameKeyDown
        ]
        []


onColumnNameKeyDown : Key -> Maybe Msg
onColumnNameKeyDown key =
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
