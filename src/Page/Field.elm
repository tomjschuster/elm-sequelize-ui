module Page.Field exposing (Model, Msg, init, initialModel, update, view)

import Data.Entity as Entity exposing (Entity)
import Data.Field as Field exposing (Field)
import Data.Schema as Schema exposing (Schema)
import Html exposing (Html, div, h2, main_, section, text)
import Http
import Request.Entity as RE
import Request.Field as RF
import Request.Schema as RS
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


type alias InitialData =
    { schema : Schema
    , entity : Entity
    , field : Field
    }


init : Int -> Int -> Int -> Cmd Msg
init schemaId entityId id =
    Task.map3 InitialData
        (RS.one schemaId |> Http.toTask)
        (RE.one entityId |> Http.toTask)
        (RF.one id |> Http.toTask)
        |> Task.attempt LoadInitialData



-- UPDATE


type Msg
    = Goto Route
    | LoadInitialData (Result Http.Error InitialData)
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

        LoadInitialData (Ok { schema, entity, field }) ->
            ( { model
                | schema = schema
                , entity = entity
                , field = field
                , error = Nothing
              }
            , Cmd.none
            )

        LoadInitialData (Err error) ->
            ( { model | error = Just "Error loading initial data" }, Cmd.none )

        -- FIELD
        LoadField (Ok field) ->
            ( { model | field = field, error = Nothing }, Cmd.none )

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
view { schema, entity, field } =
    main_ []
        [ breadcrumbs schema entity field
        , h2 [] [ text field.name ]
        ]


breadcrumbs : Schema -> Entity -> Field -> Html Msg
breadcrumbs schema entity field =
    BC.view Goto
        [ BC.home, BC.schema schema, BC.entity entity, BC.field schema.id field ]



-- FIELD VIEW


nameView : Maybe String -> String -> Html Msg
nameView editingName name =
    section [] (nameViewChildren editingName name)


nameViewChildren : Maybe String -> String -> List (Html Msg)
nameViewChildren editingName name =
    []
