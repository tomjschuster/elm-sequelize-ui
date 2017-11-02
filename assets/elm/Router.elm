module Router
    exposing
        ( Route(..)
        , fromLocation
        , getUrl
        , goto
        , link
        , replaceWith
        , routePath
        )

import Html exposing (Attribute, Html, a, p, text)
import Html.Attributes exposing (href)
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, int, s, top)
import Utils.Handlers exposing (onPreventDefaultClick)


type Route
    = Home
    | Schema Int
    | Table Int Int
    | NotFound


type alias RouteConfig =
    { route : Route
    , title : String
    , url : String
    , parent : Maybe Route
    }


getConfig : Route -> RouteConfig
getConfig route =
    case route of
        Home ->
            { route = Home
            , title = "Home"
            , url = "/schemas"
            , parent = Nothing
            }

        Schema id ->
            { route = Schema id
            , title = "Schema"
            , url = "/schemas/" ++ toString id
            , parent = Just Home
            }

        Table schemaId id ->
            { route = Table id schemaId
            , title = "Table"
            , url =
                "/schemas/"
                    ++ toString schemaId
                    ++ "/tables/"
                    ++ toString id
            , parent = Just (Schema schemaId)
            }

        NotFound ->
            { route = NotFound
            , title = "Not Found"
            , url = "/not-found"
            , parent = Nothing
            }


getUrl : Route -> String
getUrl =
    getConfig >> .url


getParent : Route -> Maybe Route
getParent =
    getConfig >> .parent


routeParser : Parser (Route -> Route) Route
routeParser =
    Url.oneOf
        [ Url.map Home top
        , Url.map Home (s "schemas")
        , Url.map Schema (s "schemas" </> int)
        , Url.map Table (s "schemas" </> int </> s "tables" </> int)
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


routePath : Route -> List Route
routePath route =
    route
        |> getParent
        |> Maybe.map (routePath >> (::) route)
        |> Maybe.withDefault []
        |> List.reverse
