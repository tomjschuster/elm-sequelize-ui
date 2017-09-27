module Router
    exposing
        ( Route(..)
        , fromLocation
        , getUrl
        , goto
        , link
        , replaceWith
        )

import Html exposing (Attribute, Html, a)
import Html.Attributes exposing (href)
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, int, s, top)
import Utils.Handlers exposing (onPreventDefaultClick)


type Route
    = Home
    | Schema Int
    | NotFound


type alias RouteConfig =
    { route : Route
    , url : String
    , parent : Maybe Route
    }


getConfig : Route -> RouteConfig
getConfig route =
    case route of
        Home ->
            { route = Home
            , url = "/schemas"
            , parent = Nothing
            }

        Schema id ->
            { route = Schema id
            , url = "/schemas/" ++ toString id
            , parent = Just Home
            }

        NotFound ->
            { route = NotFound
            , url = "/not-found"
            , parent = Nothing
            }


getUrl : Route -> String
getUrl =
    getConfig >> .url


routeParser : Parser (Route -> Route) Route
routeParser =
    Url.oneOf
        [ Url.map Home top
        , Url.map Home (s "schemas")
        , Url.map Schema (s "schemas" </> int)
        ]


fromLocation : Location -> ( Location, Route )
fromLocation location =
    location
        |> (Url.parsePath routeParser >> Maybe.withDefault NotFound >> (,) location)



-- NAV HELPERS


goto : Route -> Cmd msg
goto =
    getUrl >> Navigation.newUrl


replaceWith : Route -> Cmd msg
replaceWith =
    getUrl >> Navigation.modifyUrl


link : (Route -> msg) -> Route -> List (Attribute msg) -> List (Html msg) -> Html msg
link toMsg route attributes children =
    a
        (getHref route :: getOnClick toMsg route :: attributes)
        children


getHref : Route -> Attribute msg
getHref =
    getUrl >> href


getOnClick : (Route -> msg) -> Route -> Attribute msg
getOnClick toMsg =
    toMsg >> onPreventDefaultClick