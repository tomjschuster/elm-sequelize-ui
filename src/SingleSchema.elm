module SingleSchema exposing (Model, Msg, init, initialModel, subscriptions, update, view)

import Data exposing (Schema, emptySchema)
import Html exposing (Html, div, h1, text)


-- MODEL


type alias Model =
    { schema : Schema }


initialModel : Model
initialModel =
    Model emptySchema


init : Schema -> Model
init schema =
    Model schema



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ h1 [] [ text model.schema.name ] ]
