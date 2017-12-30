module Main exposing (main)

import AppUpdate exposing (AppUpdate)
import Html exposing (Html, div, footer, h1, header, text)
import Navigation exposing (Location)
import Page.Home as Home
import Page.Schema as Schema
import Page.Table as Table
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
    , error : Maybe String
    }


type Page
    = Home Home.Model
    | Schema Schema.Model
    | Table Table.Model


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( _, route ) =
            Router.fromLocation location

        ( page, cmd ) =
            setRoute route
    in
    ( Model ( location, route ) page Nothing, Cmd.map PageMsg cmd )



-- UPDATE


type Msg
    = SetRoute ( Location, Route )
    | PageMsg PageMsg
    | Goto Route


type PageMsg
    = HomeMsg Home.Msg
    | SchemaMsg Schema.Msg
    | TableMsg Table.Msg


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
                ( page, pageCmd, appUpdate ) =
                    updatePage pageMsg model.page
            in
            handleAppUpdate
                appUpdate
                { model | page = page }
                (Cmd.map PageMsg pageCmd)

        Goto route ->
            ( model, Router.goto route )


setRoute : Route -> ( Page, Cmd PageMsg )
setRoute route =
    case route of
        Router.Home ->
            Home.init |> mapPageInit Home HomeMsg

        Router.Schema schemaId ->
            Schema.init schemaId |> mapPageInit Schema SchemaMsg

        Router.Table schemaId tableId ->
            Table.init schemaId tableId |> mapPageInit Table TableMsg

        Router.NotFound ->
            Home.init |> mapPageInit Home HomeMsg


mapPageInit :
    (model -> Page)
    -> (msg -> PageMsg)
    -> ( model, Cmd msg )
    -> ( Page, Cmd PageMsg )
mapPageInit toPage toMsg ( model, cmd ) =
    ( toPage model, Cmd.map toMsg cmd )


updatePage : PageMsg -> Page -> ( Page, Cmd PageMsg, AppUpdate )
updatePage msg page =
    case ( msg, page ) of
        ( HomeMsg subMsg, Home subModel ) ->
            updatePageHelper Home HomeMsg Home.update subMsg subModel

        ( SchemaMsg subMsg, Schema subModel ) ->
            updatePageHelper Schema SchemaMsg Schema.update subMsg subModel

        ( TableMsg subMsg, Table subModel ) ->
            updatePageHelper Table TableMsg Table.update subMsg subModel

        ( _, _ ) ->
            ( page, Cmd.none, AppUpdate.none )


handleAppUpdate : AppUpdate -> Model -> Cmd Msg -> ( Model, Cmd Msg )
handleAppUpdate appUpdate model cmd =
    case appUpdate of
        AppUpdate.GeneralError error ->
            ( { model | error = Just error }, Cmd.none )

        AppUpdate.HttpError error ->
            ( { model | error = Just (toString error) }, cmd )

        AppUpdate.HideError ->
            ( { model | error = Nothing }, cmd )

        AppUpdate.None ->
            ( model, cmd )


updatePageHelper :
    (model -> Page)
    -> (msg -> PageMsg)
    -> (msg -> model -> ( model, Cmd msg, AppUpdate ))
    -> msg
    -> model
    -> ( Page, Cmd PageMsg, AppUpdate )
updatePageHelper toModel toMsg pageUpdate msg model =
    let
        ( updatedModel, cmd, appUpdate ) =
            pageUpdate msg model
    in
    ( toModel updatedModel, Cmd.map toMsg cmd, appUpdate )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    model |> pageView |> Html.map PageMsg |> layout model


errorView : Maybe String -> Html msg
errorView error =
    Maybe.withDefault "" error |> text


layout : Model -> Html Msg -> Html Msg
layout model content =
    div []
        [ errorView model.error
        , header []
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

        Table subModel ->
            Table.view subModel |> Html.map TableMsg
