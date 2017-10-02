module Page.Field exposing (Model, Msg, init, initialModel, update, view)

import Data.Combined as Combined exposing (FieldWithAll)
import Data.Entity as Entity exposing (Entity)
import Data.Field as Field exposing (Field)
import Data.Schema as Schema exposing (Schema)
import Html exposing (Html, button, div, h2, input, main_, section, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Http
import Request.Field as RF
import Router exposing (Route)
import Task exposing (Task)
import Views.Breadcrumbs as BC


-- MODEL


type alias Model =
    { schema : Schema
    , entity : Entity
    , field : Field
    , editingName : Maybe String
    , error : Maybe String
    }


initialModel : Model
initialModel =
    Model Schema.empty Entity.empty Field.empty Nothing Nothing


init : Int -> Int -> Int -> Cmd Msg
init schemaId entityId id =
    (RF.oneWithAll id |> Http.toTask)
        |> Task.attempt LoadFieldWithAll



-- UPDATE


type Msg
    = Goto Route
    | LoadFieldWithAll (Result Http.Error FieldWithAll)
      -- FIELD
    | LoadField (Result Http.Error Field)
    | EditFieldName
    | InputEditFieldName String
    | CancelEditFieldName
    | SaveFieldName
    | Destroy
    | RemoveField (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Goto route ->
            ( model, Router.goto route )

        LoadFieldWithAll (Ok { schema, entity, field }) ->
            ( { model
                | schema = schema
                , entity = entity
                , field = field
                , error = Nothing
              }
            , Cmd.none
            )

        LoadFieldWithAll (Err error) ->
            ( { model
                | error = Just "Error loading initial data"
                , editingName = Nothing
              }
            , Cmd.none
            )

        -- FIELD
        LoadField (Ok field) ->
            ( { model
                | field = field
                , editingName = Nothing
                , error = Nothing
              }
            , Cmd.none
            )

        LoadField (Err error) ->
            ( { model | error = Just "Error loading field" }, Cmd.none )

        EditFieldName ->
            ( { model | editingName = Just model.field.name }, Cmd.none )

        InputEditFieldName name ->
            ( { model | editingName = Just name }, Cmd.none )

        CancelEditFieldName ->
            ( { model | editingName = Nothing }, Cmd.none )

        SaveFieldName ->
            ( model
            , model.editingName
                |> Maybe.map
                    (updateFieldName model.field
                        >> RF.update
                        >> Http.send LoadField
                    )
                |> Maybe.withDefault Cmd.none
            )

        Destroy ->
            ( model, RF.destroy model.field.id |> Http.send RemoveField )

        RemoveField (Ok ()) ->
            ( model, Router.goto (Router.Entity model.schema.id model.entity.id) )

        RemoveField (Err error) ->
            ( { model | error = Just "Error deleting field" }, Cmd.none )


updateFieldName : Field -> String -> Field
updateFieldName field name =
    { field | name = name }



-- VIEW


view : Model -> Html Msg
view { schema, entity, field, editingName } =
    main_
        []
        [ breadcrumbs schema entity field
        , nameView editingName field.name
        ]


breadcrumbs : Schema -> Entity -> Field -> Html Msg
breadcrumbs schema entity field =
    BC.view Goto
        [ BC.home, BC.schema schema, BC.entity entity, BC.field schema.id field ]



-- FIELD VIEW


nameView : Maybe String -> String -> Html Msg
nameView editingName name =
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
    input [ value name, onInput InputEditFieldName ] []


cancelUpdateFieldName : Html Msg
cancelUpdateFieldName =
    button [ onClick CancelEditFieldName ] [ text "Cancel" ]


saveFieldNameButton : Html Msg
saveFieldNameButton =
    button [ onClick SaveFieldName ] [ text "Save" ]