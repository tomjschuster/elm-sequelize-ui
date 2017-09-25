module Main exposing (..)

import Home
import Html exposing (Html, div, footer, h1, header, text)
import Navigation exposing (Location)
import SingleSchema
import UrlParser as Url exposing ((</>), Parser, int, s, top)


main : Program Never Model Msg
main =
    Navigation.program (getRoute >> SetRoute)
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Route
    = HomeRoute
    | SchemaRoute Int
    | NotFound


type Page
    = Home Home.Model
    | SingleSchema SingleSchema.Model


type PageMessage
    = HomeMsg Home.Msg
    | SingleSchemaMsg SingleSchema.Msg


routeParser : Parser (Route -> Route) Route
routeParser =
    Url.oneOf
        [ Url.map HomeRoute top
        , Url.map HomeRoute (s "home")
        , Url.map SchemaRoute (s "schema" </> int)
        ]


getRoute : Location -> Route
getRoute =
    Url.parsePath routeParser >> Maybe.withDefault NotFound


routeToPage : Route -> Page
routeToPage route =
    case route of
        HomeRoute ->
            Home Home.initialModel

        SchemaRoute id ->
            SingleSchema SingleSchema.initialModel

        NotFound ->
            Home Home.initialModel



-- MODEL


type alias Model =
    { navState : ( Location, Route )
    , page : Page
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            getRoute location
    in
    Model ( location, route ) (routeToPage route) ! []



-- UPDATE


type Msg
    = SetRoute Route
    | PageMsg PageMessage


updatePage :
    (model -> Page)
    -> (msg -> PageMessage)
    -> (msg -> model -> ( model, Cmd msg ))
    -> msg
    -> model
    -> ( Page, Cmd PageMessage )
updatePage toModel toMsg pageUpdate msg model =
    let
        ( updatedModel, cmd ) =
            pageUpdate msg model
    in
    ( toModel updatedModel, Cmd.map toMsg cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoute route ->
            ( { model | page = routeToPage route }, Cmd.none )

        PageMsg pageMsg ->
            let
                ( page, pageCmd ) =
                    case pageMsg of
                        HomeMsg subMsg ->
                            case model.page of
                                Home subModel ->
                                    updatePage Home HomeMsg Home.update subMsg subModel

                                _ ->
                                    ( model.page, Cmd.none )

                        SingleSchemaMsg subMsg ->
                            case model.page of
                                SingleSchema subModel ->
                                    updatePage SingleSchema SingleSchemaMsg SingleSchema.update subMsg subModel

                                _ ->
                                    ( model.page, Cmd.none )
            in
            ( { model | page = page }, Cmd.map PageMsg pageCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ header []
            [ h1 [] [ text "Sequelize UI" ] ]
        , pageView model
        , footer []
            []
        ]


pageView : Model -> Html Msg
pageView model =
    case model.page of
        Home subModel ->
            Home.view subModel |> Html.map (HomeMsg >> PageMsg)

        SingleSchema subModel ->
            SingleSchema.view subModel |> Html.map (SingleSchemaMsg >> PageMsg)
