module Page.Field exposing (Model, Msg, init, initialModel, update, view)

import AppUpdate exposing (AppUpdate)
import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Data.Combined as Combined exposing (FieldWithAll)
import Data.Entity as Entity exposing (Entity)
import Data.Field as Field exposing (Field)
import Data.Schema as Schema exposing (Schema)
import Data.DataType as DataType exposing (DataType)
import Dom
import Html
    exposing
        ( Html
        , button
        , div
        , h2
        , input
        , main_
        , section
        , text
        , select
        , option
        )
import Html.Attributes exposing (id, value)
import Html.Events exposing (onClick, onInput)
import Http
import Request.Field as RF
import Router exposing (Route)
import Task exposing (Task)
import Utils.Handlers exposing (customOnKeyDown, onEnter, onEscape)
import Utils.Keys exposing (Key(..))
import Views.Breadcrumbs as BC


-- MODEL


type alias Model =
    { schema : Schema
    , entity : Entity
    , field : Field
    , editingName : Maybe String
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    Model Schema.empty Entity.empty Field.empty Nothing []


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
    | EditFieldName
    | InputEditFieldName String
    | CancelEditFieldName
    | SaveFieldName
    | Destroy
    | RemoveField (Result Http.Error ())


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

        EditFieldName ->
            ( { model | editingName = Just model.field.name }
            , Dom.focus "edit-field-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        InputEditFieldName name ->
            ( { model | editingName = Just name }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditFieldName ->
            ( { model | editingName = Nothing }
            , Cmd.none
            , AppUpdate.none
            )

        SaveFieldName ->
            ( model
            , model.editingName
                |> Maybe.map
                    (updateFieldName model.field
                        >> RF.update
                        >> Http.send LoadField
                    )
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


updateFieldName : Field -> String -> Field
updateFieldName field name =
    { field | name = name }



-- VIEW


view : Model -> Html Msg
view { schema, entity, field, editingName } =
    main_
        []
        [ breadcrumbs schema entity field
        , title editingName field.name
        ]


breadcrumbs : Schema -> Entity -> Field -> Html Msg
breadcrumbs schema entity field =
    BC.view Goto
        [ BC.home, BC.schema schema, BC.entity entity, BC.field schema.id field ]



-- FIELD VIEW


title : Maybe String -> String -> Html Msg
title editingName name =
    section [] (nameViewChildren name editingName)


nameViewChildren : String -> Maybe String -> List (Html Msg)
nameViewChildren name =
    Maybe.map editingNameChildren
        >> Maybe.withDefault (normalNameChildren name)


editingNameChildren : String -> List (Html Msg)
editingNameChildren name =
    [ fieldNameInput name, saveFieldNameButton, cancelUpdateFieldName ]


normalNameChildren : String -> List (Html Msg)
normalNameChildren name =
    [ nameTitle name, editFieldNameButton, deleteFieldButton ]


nameTitle : String -> Html Msg
nameTitle name =
    h2 [] [ text name ]


editFieldNameButton : Html Msg
editFieldNameButton =
    button [ onClick EditFieldName ] [ text "Edit Field Name" ]


deleteFieldButton : Html Msg
deleteFieldButton =
    button [ onClick Destroy ] [ text "Delete" ]


fieldNameInput : String -> Html Msg
fieldNameInput name =
    input
        [ id "edit-field-name"
        , value name
        , onInput InputEditFieldName
        , customOnKeyDown onFieldNameKeyDown
        ]
        []


onFieldNameKeyDown : Key -> Maybe Msg
onFieldNameKeyDown key =
    case key of
        Enter ->
            Just SaveFieldName

        Escape ->
            Just CancelEditFieldName

        _ ->
            Nothing


cancelUpdateFieldName : Html Msg
cancelUpdateFieldName =
    button [ onClick CancelEditFieldName ] [ text "Cancel" ]


saveFieldNameButton : Html Msg
saveFieldNameButton =
    button [ onClick SaveFieldName ] [ text "Save" ]



-- DataType


dataTypeDropDown : Html Msg
dataTypeDropDown =
    select [] []
