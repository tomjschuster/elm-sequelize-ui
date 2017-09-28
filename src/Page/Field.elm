module Page.Field exposing (Model, Msg, init, initialModel, update, view)

import Data.Entity as Entity exposing (Entity)
import Data.Field as Field exposing (Field)
import Data.Schema as Schema exposing (Schema)
import Html exposing (Html, div, h2, main_, text)
import Http
import Request.Entity as RE
import Request.Field as RF
import Request.Schema as RS
import Router exposing (Route)
import Task exposing (Task)


-- MODEL


type alias Model =
    { schema : Schema
    , entity : Entity
    , field : Field
    , error : Maybe String
    }


initialModel : Model
initialModel =
    Model Schema.empty Entity.empty Field.empty Nothing


type alias InitialData =
    { schema : Schema
    , entity : Entity
    , field : Field
    }


init : Int -> Int -> Int -> Cmd Msg
init schemaId entityId id =
    Task.map3 InitialData
        (RS.one id |> Http.toTask)
        (RE.one id |> Http.toTask)
        (RF.one id |> Http.toTask)
        |> Task.attempt LoadInitialData



-- UPDATE


type Msg
    = Goto Route
    | LoadInitialData (Result Http.Error InitialData)
    | LoadField (Result Http.Error Field)
    | LoadEntity (Result Http.Error Entity)


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

        LoadField (Ok field) ->
            ( { model | field = field, error = Nothing }
            , RE.one field.entityId
                |> Http.send LoadEntity
            )

        LoadField (Err error) ->
            ( { model | error = Just "Error loading field" }, Cmd.none )

        LoadEntity (Ok entity) ->
            ( { model | entity = entity, error = Nothing }, Cmd.none )

        LoadEntity (Err error) ->
            ( { model | error = Just "Error loading entity" }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ entityLink model.entity
        , h2 [] [ text model.field.name ]
        ]


entityLink : Entity -> Html Msg
entityLink { id, schemaId, name } =
    Router.link Goto (Router.Entity schemaId id) [] [ text name ]
