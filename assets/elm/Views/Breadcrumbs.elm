module Views.Breadcrumbs exposing (field, home, schema, table, view)

import Data.Field as Field exposing (Field)
import Data.Schema as Schema exposing (Schema)
import Data.Table as Table exposing (Table)
import Html exposing (Html, p, text)
import Router exposing (Route)


view : (Route -> msg) -> List ( Route, String ) -> Html msg
view gotoMsg crumbs =
    p [] (breadcrumbsChildren gotoMsg crumbs)


breadcrumbsChildren : (Route -> msg) -> List ( Route, String ) -> List (Html msg)
breadcrumbsChildren gotoMsg crumbs =
    case List.reverse crumbs of
        ( route, title ) :: ancestors ->
            (text title :: List.map (breadcrumb gotoMsg) ancestors)
                |> List.reverse
                |> List.intersperse (text " > ")

        [] ->
            []


breadcrumb : (Route -> msg) -> ( Route, String ) -> Html msg
breadcrumb gotoMsg ( route, name ) =
    Router.link gotoMsg route [] [ text name ]


home : ( Route, String )
home =
    ( Router.Home, "Home" )


schema : Schema -> ( Route, String )
schema { id, name } =
    ( Router.Schema id, name )


table : Table -> ( Route, String )
table { id, name, schemaId } =
    ( Router.Table schemaId id, name )


field : Int -> Field -> ( Route, String )
field schemaId { id, name, tableId } =
    ( Router.Field schemaId tableId id, name )
