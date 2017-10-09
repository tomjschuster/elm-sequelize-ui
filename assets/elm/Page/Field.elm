module Page.Field exposing (Model, Msg, init, initialModel, update, view)

import AppUpdate exposing (AppUpdate)
import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Data.Combined as Combined exposing (FieldWithAll)
import Data.DataType as DataType exposing (DataType)
import Data.Entity as Entity exposing (Entity)
import Data.Field as Field exposing (Field)
import Data.Schema as Schema exposing (Schema)
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
import Request.Field as RF
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


-- MODEL


type alias Model =
    { schema : Schema
    , entity : Entity
    , field : Field
    , editing : Bool
    , editingName : Maybe String
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    Model Schema.empty Entity.empty Field.empty False Nothing []


init : Int -> Int -> Int -> Cmd Msg
init schemaId entityId id =
    (RF.oneWithAll id |> Http.toTask)
        |> Task.attempt LoadFieldWithAll



-- UPDATE


type Msg
    = NoOp
    | FocusResult (Result Dom.Error ())
    | Goto Route
    | LoadFieldWithAll (Result Http.Error FieldWithAll)
      -- FIELD
    | LoadField (Result Http.Error Field)
    | EditField
    | InputFieldName String
    | SelectDataType (Maybe Int)
    | UpdateSize (Maybe Int)
    | UpdatePrecision (Maybe Int) (Maybe Int)
    | UpdateWithTimezone Bool
    | CancelEditField
    | SaveField
    | Destroy
    | RemoveField (Result Http.Error ())
      -- DataType
    | UpdateDataType DataType


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

        LoadFieldWithAll (Ok { schema, entity, field }) ->
            ( { model
                | schema = schema
                , entity = entity
                , field = field
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadFieldWithAll (Err error) ->
            ( { model
                | errors = ChangesetError.parseHttpError error
                , editingName = Nothing
              }
            , Cmd.none
            , AppUpdate.none
            )

        -- FIELD
        LoadField (Ok field) ->
            ( { model
                | field = field
                , editingName = Nothing
                , editing = False
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadField (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        EditField ->
            ( { model | editingName = Just model.field.name, editing = True }
            , Dom.focus "edit-field-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        InputFieldName name ->
            ( { model | editingName = Just name, field = updateFieldName model.field name }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditField ->
            ( { model | editingName = Nothing }
            , RF.one model.field.id |> Http.send LoadField
            , AppUpdate.none
            )

        SelectDataType maybeId ->
            let
                dataType =
                    maybeId
                        |> Maybe.andThen DataType.fromId
                        |> Maybe.withDefault DataType.none
            in
            ( { model
                | field =
                    model.field
                        |> Field.updateDataType dataType
                        |> Field.updateDataTypeModifier
                            (DataType.toInitialModifier dataType)
              }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateSize size ->
            let
                modifier =
                    model.field.dataTypeModifier |> DataType.updateSize size
            in
            ( { model | field = Field.updateDataTypeModifier modifier model.field }
            , Cmd.none
            , AppUpdate.none
            )

        UpdatePrecision precision decimal ->
            let
                modifier =
                    model.field.dataTypeModifier |> DataType.updatePrecision precision decimal
            in
            ( { model | field = Field.updateDataTypeModifier modifier model.field }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateWithTimezone withTimezone ->
            let
                modifier =
                    model.field.dataTypeModifier |> DataType.updateWithTimezone withTimezone
            in
            ( { model | field = Field.updateDataTypeModifier modifier model.field }
            , Cmd.none
            , AppUpdate.none
            )

        SaveField ->
            ( model
            , RF.update model.field |> Http.send LoadField
            , AppUpdate.none
            )

        Destroy ->
            ( model
            , RF.destroy model.field.id |> Http.send RemoveField
            , AppUpdate.none
            )

        RemoveField (Ok ()) ->
            ( model
            , Router.goto (Router.Entity model.schema.id model.entity.id)
            , AppUpdate.none
            )

        RemoveField (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        -- DATA TYPE
        UpdateDataType dataType ->
            ( { model | field = Field.updateDataType dataType model.field }
            , Cmd.none
            , AppUpdate.none
            )


updateFieldName : Field -> String -> Field
updateFieldName field name =
    { field | name = name }



-- VIEW


view : Model -> Html Msg
view { schema, entity, field, editing, errors } =
    main_
        []
        [ breadcrumbs schema entity field
        , fieldButtons editing
        , div [] (CE.prependIfErrors errors [])
        , nameView editing field.name
        ]


breadcrumbs : Schema -> Entity -> Field -> Html Msg
breadcrumbs schema entity field =
    BC.view Goto
        [ BC.home, BC.schema schema, BC.entity entity, BC.field schema.id field ]



-- NAME


cancelUpdateField : Html Msg
cancelUpdateField =
    button [ onClick CancelEditField ] [ text "Cancel" ]


saveFieldButton : Html Msg
saveFieldButton =
    button [ onClick SaveField ] [ text "Save" ]


nameView : Bool -> String -> Html Msg
nameView editing name =
    section [] (nameViewChildren name editing)


nameViewChildren : String -> Bool -> List (Html Msg)
nameViewChildren name editing =
    if editing then
        editingNameChildren name
    else
        normalNameChildren name


fieldButtons : Bool -> Html Msg
fieldButtons editing =
    if editing then
        div [] [ saveFieldButton, cancelUpdateField ]
    else
        div [] [ editFieldNameButton, deleteFieldButton ]


editingNameChildren : String -> List (Html Msg)
editingNameChildren name =
    [ fieldNameInput name ]


normalNameChildren : String -> List (Html Msg)
normalNameChildren name =
    [ nameTitle name ]


nameTitle : String -> Html Msg
nameTitle name =
    h2 [] [ text name ]


editFieldNameButton : Html Msg
editFieldNameButton =
    button [ onClick EditField ] [ text "Edit" ]


deleteFieldButton : Html Msg
deleteFieldButton =
    button [ onClick Destroy ] [ text "Delete" ]


fieldNameInput : String -> Html Msg
fieldNameInput name =
    input
        [ id "edit-field-name"
        , value name
        , onInput InputFieldName
        , customOnKeyDown onFieldNameKeyDown
        ]
        []


onFieldNameKeyDown : Key -> Maybe Msg
onFieldNameKeyDown key =
    case key of
        Enter ->
            Just SaveField

        Escape ->
            Just CancelEditField

        _ ->
            Nothing
