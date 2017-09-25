module SingleSchema exposing (Model, Msg, init, initialModel, subscriptions, update, view)

import Data exposing (Schema, emptySchema)
import Html exposing (Html, div, h2, text)
import Http
import Request


-- MODEL


type alias Model =
    { schema : Schema
    , error : Maybe String
    }


initialModel : Model
initialModel =
    Model emptySchema Nothing


init : Int -> Cmd Msg
init id =
    Request.getSchema id |> Http.send LoadSchema



-- UPDATE


type Msg
    = LoadSchema (Result Http.Error Schema)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadSchema (Ok schema) ->
            ( { model | schema = schema, error = Nothing }, Cmd.none )

        LoadSchema (Err error) ->
            ( { model | error = Just "Error loading schema" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ h2 [] [ text model.schema.name ] ]
