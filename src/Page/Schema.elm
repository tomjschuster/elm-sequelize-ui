module Page.Schema exposing (Model, Msg, init, initialModel, subscriptions, update, view)

import Data.Schema exposing (Schema, emptySchema)
import Html exposing (Html, a, button, div, h2, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Http
import Request.Schema as RS
import Router


-- MODEL


type alias Model =
    { schema : Schema
    , editingName : Maybe String
    , error : Maybe String
    }


initialModel : Model
initialModel =
    Model emptySchema Nothing Nothing


init : Int -> Cmd Msg
init id =
    RS.get id |> Http.send LoadSchema



-- UPDATE


type Msg
    = LoadSchema (Result Http.Error Schema)
    | RemoveSchema (Result Http.Error ())
    | EditName
    | UpdateEditingName String
    | CancelEditName
    | SaveName
    | Destroy


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadSchema (Ok schema) ->
            ( { model | schema = schema, editingName = Nothing, error = Nothing }, Cmd.none )

        LoadSchema (Err error) ->
            ( { model | error = Just "Error loading schema" }, Cmd.none )

        RemoveSchema (Ok ()) ->
            ( model, Router.goto Router.Home )

        RemoveSchema (Err error) ->
            ( { model | error = Just "Error deleting schema" }, Cmd.none )

        EditName ->
            ( { model | editingName = Just model.schema.name }, Cmd.none )

        UpdateEditingName name ->
            ( { model | editingName = Just name }, Cmd.none )

        CancelEditName ->
            ( { model | editingName = Nothing }, Cmd.none )

        SaveName ->
            ( model
            , model.editingName |> saveSchemaName model.schema |> RS.update |> Http.send LoadSchema
            )

        Destroy ->
            ( model, RS.destroy model.schema.id |> Http.send RemoveSchema )


saveSchemaName : Schema -> Maybe String -> Schema
saveSchemaName schema editingName =
    editingName
        |> Maybe.map (updateSchemaName schema)
        |> Maybe.withDefault schema


updateSchemaName : Schema -> String -> Schema
updateSchemaName schema name =
    { schema | name = name }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ title model.editingName model.schema.name
        ]


title : Maybe String -> String -> Html Msg
title maybeEditingName name =
    case maybeEditingName of
        Just editingName ->
            editTitle editingName

        Nothing ->
            viewTitle name


viewTitle : String -> Html Msg
viewTitle name =
    div []
        [ h2 [] [ text name ]
        , button [ onClick EditName ] [ text "Edit Title" ]
        , button [ onClick Destroy ] [ text "Delete Schema" ]
        ]


editTitle : String -> Html Msg
editTitle name =
    div []
        [ input [ value name, onInput UpdateEditingName ] []
        , button [ onClick CancelEditName ] [ text "Cancel" ]
        , button [ onClick SaveName ] [ text "Save" ]
        ]
