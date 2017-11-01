module Page.Column exposing (Model, Msg, init, initialModel, update, view)

import AppUpdate exposing (AppUpdate)
import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Data.Column as Column exposing (Column)
import Data.Combined as Combined exposing (ColumnWithAll)
import Data.DataType as DataType exposing (DataType)
import Data.Schema as Schema exposing (Schema)
import Data.Table as Table exposing (Table)
import Dom
import Html
    exposing
        ( Html
        , button
        , div
        , h2
        , h3
        , input
        , main_
        , option
        , section
        , select
        , text
        )
import Html.Attributes exposing (id, selected, value)
import Html.Events exposing (onClick, onInput)
import Http
import Request.Column as RF
import Router exposing (Route)
import Task exposing (Task)
import Utils.Handlers
    exposing
        ( customOnKeyDown
        , onEnter
        , onEscape
        , onPreventDefaultClick
        )
import Utils.Keys exposing (Key(..))
import Views.Breadcrumbs as BC
import Views.ChangesetError as CE
import Views.Column.DataTypeDisplay as DTDisplay
import Views.Column.DataTypeSelect as DTSelect


-- MODEL


type alias Model =
    { schema : Schema
    , table : Table
    , column : Column
    , editingColumn : Maybe Column
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    Model Schema.empty Table.empty Column.empty Nothing []


init : Int -> Int -> Int -> ( Model, Cmd Msg )
init schemaId tableId id =
    ( initialModel
    , Cmd.none
      --, (RF.oneWithAll id |> Http.toTask) |> Task.attempt LoadColumnWithAll
    )



-- UPDATE


type Msg
    = NoOp
    | FocusResult (Result Dom.Error ())
    | Goto Route
    | LoadColumnWithAll (Result Http.Error ColumnWithAll)
      -- FIELD
    | LoadColumn (Result Http.Error Column)
    | EditColumn
    | InputColumnName String
    | UpdateDataType DataType
    | CancelEditColumn
    | SaveColumn
    | Destroy
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
            ( model, Router.goto route, AppUpdate.none )

        LoadColumnWithAll (Ok { schema, table, column }) ->
            ( { model
                | schema = schema
                , table = table
                , column = column
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadColumnWithAll (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        -- FIELD
        LoadColumn (Ok column) ->
            ( { model
                | column = column
                , editingColumn = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadColumn (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        EditColumn ->
            ( { model | editingColumn = Just model.column }
            , Dom.focus "edit-column-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        InputColumnName name ->
            ( { model
                | editingColumn =
                    Maybe.map (Column.updateName name) model.editingColumn
              }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditColumn ->
            ( { model | editingColumn = Nothing }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateDataType dataType ->
            ( { model
                | editingColumn =
                    Maybe.map (Column.updateDataType dataType) model.editingColumn
              }
            , Cmd.none
            , AppUpdate.none
            )

        SaveColumn ->
            ( model
            , model.editingColumn
                |> Maybe.map (RF.update >> Http.send LoadColumn)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        Destroy ->
            ( model
            , RF.destroy model.column.id |> Http.send RemoveColumn
            , AppUpdate.none
            )

        RemoveColumn (Ok ()) ->
            ( model
            , Router.goto (Router.Table model.schema.id model.table.id)
            , AppUpdate.none
            )

        RemoveColumn (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )


updateColumnName : Column -> String -> Column
updateColumnName column name =
    { column | name = name }



-- VIEW


view : Model -> Html Msg
view { schema, table, column, editingColumn, errors } =
    main_
        []
        [ breadcrumbs schema table column
        , buttons editingColumn
        , div [] (CE.prependIfErrors errors [])
        , columnView editingColumn column
        ]


breadcrumbs : Schema -> Table -> Column -> Html Msg
breadcrumbs schema table column =
    BC.view Goto
        [ BC.home, BC.schema schema, BC.table table, BC.column schema.id column ]



-- BUTTONS


buttons : Maybe Column -> Html Msg
buttons editingColumn =
    if editingColumn == Nothing then
        div [] [ editColumnNameButton, deleteColumnButton ]
    else
        div [] [ saveColumnButton, cancelUpdateColumn ]


saveColumnButton : Html Msg
saveColumnButton =
    button [ onClick SaveColumn ] [ text "Save" ]


cancelUpdateColumn : Html Msg
cancelUpdateColumn =
    button [ onClick CancelEditColumn ] [ text "Cancel" ]


editColumnNameButton : Html Msg
editColumnNameButton =
    button [ onClick EditColumn ] [ text "Edit" ]


deleteColumnButton : Html Msg
deleteColumnButton =
    button [ onClick Destroy ] [ text "Delete" ]



-- FIELD VIEW


columnView : Maybe Column -> Column -> Html Msg
columnView editingColumn column =
    section [] (columnChildren editingColumn column)


columnChildren : Maybe Column -> Column -> List (Html Msg)
columnChildren editingColumn column =
    editingColumn
        |> Maybe.map editingColumnChildren
        |> Maybe.withDefault (readColumnChildren column)



-- READ FIELD


readColumnChildren : Column -> List (Html Msg)
readColumnChildren { name, dataType } =
    [ nameTitle name, DTDisplay.view dataType ]


nameTitle : String -> Html Msg
nameTitle name =
    h2 [] [ text name ]



-- UPDATE FIELD


editingColumnChildren : Column -> List (Html Msg)
editingColumnChildren { name, dataType } =
    [ columnNameInput name, DTSelect.view "edit-column" UpdateDataType dataType ]


columnNameInput : String -> Html Msg
columnNameInput name =
    input
        [ id "edit-column-name"
        , value name
        , onInput InputColumnName
        , customOnKeyDown onColumnNameKeyDown
        ]
        []


onColumnNameKeyDown : Key -> Maybe Msg
onColumnNameKeyDown key =
    case key of
        Enter ->
            Just SaveColumn

        Escape ->
            Just CancelEditColumn

        _ ->
            Nothing
