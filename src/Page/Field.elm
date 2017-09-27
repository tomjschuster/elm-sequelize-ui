module Page.Field exposing (Model, Msg, init, initialModel, update, view)

import Data.Entity as Entity exposing (Entity)
import Data.Field as Field exposing (Field)
import Html exposing (Html, div, h2, main_, text)
import Http
import Request.Entity as RE
import Request.Field as RF
import Router exposing (Route)


type alias Model =
    { field : Field
    , entity : Entity
    , error : Maybe String
    }


initialModel : Model
initialModel =
    Model Field.empty Entity.empty Nothing


init : Int -> Cmd Msg
init id =
    RF.one id |> Http.send LoadField


type Msg
    = Goto Route
    | LoadField (Result Http.Error Field)
    | LoadEntity (Result Http.Error Entity)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Goto route ->
            ( model, Router.goto route )

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


view : Model -> Html Msg
view model =
    main_ []
        [ entityLink model.entity
        , h2 [] [ text model.field.name ]
        ]


entityLink : Entity -> Html Msg
entityLink { id, schemaId, name } =
    Router.link Goto (Router.Entity schemaId id) [] [ text name ]
