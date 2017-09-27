module Main exposing (..)

import Html exposing (Html, div, footer, h1, header, text)
import Navigation exposing (Location)
import Page.Home as Home
import Page.Schema as Schema
import Router exposing (Route)


main : Program Never Model Msg
main =
    Navigation.program (Router.fromLocation >> SetRoute)
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { navState : ( Location, Route )
    , page : Page
    }


type Page
    = Home Home.Model
    | Schema Schema.Model


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( _, route ) =
            Router.fromLocation location

        ( page, cmd ) =
            setRoute route
    in
    ( Model ( location, route ) page, Cmd.map PageMsg cmd )



-- UPDATE


type Msg
    = SetRoute ( Location, Route )
    | PageMsg PageMsg
    | Goto Route


type PageMsg
    = HomeMsg Home.Msg
    | SchemaMsg Schema.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoute ( location, route ) ->
            let
                ( page, cmd ) =
                    setRoute route
            in
            ( { model
                | page = page
                , navState = ( location, route )
              }
            , Cmd.map PageMsg cmd
            )

        PageMsg pageMsg ->
            let
                ( page, pageCmd ) =
                    updatePage pageMsg model.page
            in
            ( { model | page = page }, Cmd.map PageMsg pageCmd )

        Goto route ->
            ( model, Router.goto route )


setRoute : Route -> ( Page, Cmd PageMsg )
setRoute route =
    case route of
        Router.Home ->
            ( Home Home.initialModel, Home.init |> Cmd.map HomeMsg )

        Router.Schema id ->
            ( Schema Schema.initialModel, Schema.init id |> Cmd.map SchemaMsg )

        Router.NotFound ->
            ( Home Home.initialModel, Home.init |> Cmd.map HomeMsg )


updatePage : PageMsg -> Page -> ( Page, Cmd PageMsg )
updatePage msg page =
    case ( msg, page ) of
        ( HomeMsg subMsg, Home subModel ) ->
            updatePageHelper Home HomeMsg Home.update subMsg subModel

        ( SchemaMsg subMsg, Schema subModel ) ->
            updatePageHelper Schema SchemaMsg Schema.update subMsg subModel

        ( _, _ ) ->
            ( page, Cmd.none )


updatePageHelper :
    (model -> Page)
    -> (msg -> PageMsg)
    -> (msg -> model -> ( model, Cmd msg ))
    -> msg
    -> model
    -> ( Page, Cmd PageMsg )
updatePageHelper toModel toMsg pageUpdate msg model =
    let
        ( updatedModel, cmd ) =
            pageUpdate msg model
    in
    ( toModel updatedModel, Cmd.map toMsg cmd )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view =
    pageView >> Html.map PageMsg >> layout


layout : Html Msg -> Html Msg
layout content =
    div []
        [ header []
            [ headerLink ]
        , content
        , footer []
            []
        ]


headerLink : Html Msg
headerLink =
    Router.link Goto Router.Home [] [ h1 [] [ text "Sequelize UI" ] ]


pageView : Model -> Html PageMsg
pageView model =
    case model.page of
        Home subModel ->
            Home.view subModel |> Html.map HomeMsg

        Schema subModel ->
            Schema.view subModel |> Html.map SchemaMsg
