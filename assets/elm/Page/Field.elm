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
    , editingField : Maybe Field
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    Model Schema.empty Entity.empty Field.empty Nothing []


init : Int -> Int -> Int -> ( Model, Cmd Msg )
init schemaId entityId id =
    ( initialModel
    , (RF.oneWithAll id |> Http.toTask) |> Task.attempt LoadFieldWithAll
    )



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
    | SelectDataType DataType
    | UpdateModifier DataType.Modifier
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
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        -- FIELD
        LoadField (Ok field) ->
            ( { model
                | field = field
                , editingField = Nothing
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
            ( { model | editingField = Just model.field }
            , Dom.focus "edit-field-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        InputFieldName name ->
            ( { model | editingField = Maybe.map (Field.updateName name) model.editingField }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditField ->
            ( { model | editingField = Nothing }
            , Cmd.none
            , AppUpdate.none
            )

        SelectDataType dataType ->
            ( { model | editingField = Maybe.map (Field.updateDataType dataType) model.editingField }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateModifier modifier ->
            ( { model | editingField = Maybe.map (Field.updateDataTypeModifier modifier) model.editingField }
            , Cmd.none
            , AppUpdate.none
            )

        SaveField ->
            ( model
            , model.editingField
                |> Maybe.map (RF.update >> Http.send LoadField)
                |> Maybe.withDefault Cmd.none
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
view { schema, entity, field, editingField, errors } =
    main_
        []
        [ breadcrumbs schema entity field
        , buttons editingField
        , div [] (CE.prependIfErrors errors [])
        , fieldView editingField field
        ]


breadcrumbs : Schema -> Entity -> Field -> Html Msg
breadcrumbs schema entity field =
    BC.view Goto
        [ BC.home, BC.schema schema, BC.entity entity, BC.field schema.id field ]



-- BUTTONS


buttons : Maybe Field -> Html Msg
buttons editingField =
    if editingField == Nothing then
        div [] [ editFieldNameButton, deleteFieldButton ]
    else
        div [] [ saveFieldButton, cancelUpdateField ]


saveFieldButton : Html Msg
saveFieldButton =
    button [ onClick SaveField ] [ text "Save" ]


cancelUpdateField : Html Msg
cancelUpdateField =
    button [ onClick CancelEditField ] [ text "Cancel" ]


editFieldNameButton : Html Msg
editFieldNameButton =
    button [ onClick EditField ] [ text "Edit" ]


deleteFieldButton : Html Msg
deleteFieldButton =
    button [ onClick Destroy ] [ text "Delete" ]



-- FIELD VIEW


fieldView : Maybe Field -> Field -> Html Msg
fieldView editingField field =
    section [] (fieldChildren editingField field)


fieldChildren : Maybe Field -> Field -> List (Html Msg)
fieldChildren editingField field =
    editingField
        |> Maybe.map editingFieldChildren
        |> Maybe.withDefault (readFieldChildren field)



-- READ FIELD


readFieldChildren : Field -> List (Html Msg)
readFieldChildren { name } =
    [ nameTitle name ]


nameTitle : String -> Html Msg
nameTitle name =
    h2 [] [ text name ]



-- UPDATE FIELD


editingFieldChildren : Field -> List (Html Msg)
editingFieldChildren { name } =
    [ fieldNameInput name ]


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
