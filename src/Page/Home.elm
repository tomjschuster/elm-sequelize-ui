module Page.Home exposing (Model, Msg, init, initialModel, subscriptions, update, view)

import Data.Schema exposing (Schema, emptySchema, schemaDecoder)
import Html exposing (Html, a, aside, button, div, h2, input, li, main_, p, text, ul)
import Html.Attributes exposing (href, value)
import Html.Events as Events exposing (onClick, onInput)
import Http
import Request.Schema as RS
import Router exposing (Route)
import Task exposing (Task)


-- MODEL


type alias Model =
    { schemas : List Schema
    , schemaNameInput : String
    , editingSchema : Maybe Schema
    , error : Maybe String
    , nextId : Int
    , toDeleteId : Maybe Int
    }


initialModel : Model
initialModel =
    Model [] "" Nothing Nothing 1 Nothing


init : Cmd Msg
init =
    Http.send LoadSchemas RS.getAll



-- UPDATE


type Msg
    = Goto Route
    | LoadSchemas (Result Http.Error (List Schema))
    | CreateSchema (Result String Schema)
    | LoadNewSchema (Result Http.Error Schema)
    | UpdateSchema (Result String Schema)
    | LoadUpdatedSchema (Result Http.Error Schema)
    | DeleteSchema Int
    | RemoveSchema (Result Http.Error ())
    | InputSchemaName String
    | EditSchema Int
    | InputEditingSchemaName String
    | ValidateNewSchema
    | ValidateUpdatedSchema


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Goto route ->
            ( model, Router.goto route )

        CreateSchema (Ok _) ->
            ( { model | schemaNameInput = "" }, RS.create model.schemaNameInput |> Http.send LoadNewSchema )

        CreateSchema (Err error) ->
            ( { model | error = Just error }, Cmd.none )

        LoadNewSchema (Ok schema) ->
            ( { model | schemas = model.schemas ++ [ schema ], error = Nothing }, Cmd.none )

        LoadNewSchema (Err error) ->
            ( { model | error = Just "Error creating schema" }, Cmd.none )

        LoadSchemas (Ok schemas) ->
            ( { model | schemas = schemas, error = Nothing }, Cmd.none )

        LoadSchemas (Err error) ->
            ( { model | error = Just "Error loading schemas" }, Cmd.none )

        UpdateSchema (Ok _) ->
            case model.editingSchema of
                Just schema ->
                    ( { model | error = Nothing }, RS.update schema |> Http.send LoadUpdatedSchema )

                Nothing ->
                    model ! []

        UpdateSchema (Err error) ->
            ( { model | error = Just error }, Cmd.none )

        LoadUpdatedSchema (Ok schema) ->
            ( { model
                | error = Nothing
                , editingSchema = Nothing
                , schemas =
                    List.map
                        (\s ->
                            if s.id == schema.id then
                                schema
                            else
                                s
                        )
                        model.schemas
              }
            , Cmd.none
            )

        LoadUpdatedSchema (Err error) ->
            ( { model | error = Just "Error editing schema" }, Cmd.none )

        DeleteSchema id ->
            ( { model | toDeleteId = Just id }, RS.destroy id |> Http.send RemoveSchema )

        RemoveSchema (Ok ()) ->
            ( { model
                | schemas = List.filter (.id >> Just >> (/=) model.toDeleteId) model.schemas
                , toDeleteId = Nothing
                , error = Nothing
              }
            , Cmd.none
            )

        RemoveSchema (Err error) ->
            ( { model | error = Just "Error deleting schema", toDeleteId = Nothing }, Cmd.none )

        InputSchemaName name ->
            ( { model | schemaNameInput = name }, Cmd.none )

        EditSchema id ->
            let
                schema =
                    model.schemas |> List.filter (.id >> (==) id) |> List.head
            in
            ( { model | editingSchema = schema }, Cmd.none )

        InputEditingSchemaName name ->
            ( { model | editingSchema = Maybe.map (\s -> { s | name = name }) model.editingSchema }, Cmd.none )

        ValidateNewSchema ->
            ( model, validateSchema model.schemas (draftSchema model.schemaNameInput) |> Task.attempt CreateSchema )

        ValidateUpdatedSchema ->
            case model.editingSchema of
                Just schema ->
                    ( model, validateSchema model.schemas schema |> Task.attempt UpdateSchema )

                Nothing ->
                    model ! []


schemaUrl : Int -> String
schemaUrl =
    toString >> (++) "/schema/"


draftSchema : String -> Schema
draftSchema name =
    { emptySchema | name = name }


validateSchema : List Schema -> Schema -> Task String Schema
validateSchema schemas schema =
    Task.succeed schema
        |> Task.andThen validateNameExists
        |> Task.andThen (validateNameNotTaken schemas)


validateNameExists : Schema -> Task String Schema
validateNameExists schema =
    if schema.name == "" then
        Task.fail "Please enter a name"
    else
        Task.succeed schema


validateNameNotTaken : List Schema -> Schema -> Task String Schema
validateNameNotTaken schemas schema =
    if nameTaken schema schemas then
        Task.fail ("Schema named '" ++ schema.name ++ "' alread exists.")
    else
        Task.succeed schema


nameTaken : Schema -> List Schema -> Bool
nameTaken schema =
    List.filter (nameConflict schema) >> List.head >> (/=) Nothing


nameConflict : Schema -> Schema -> Bool
nameConflict current schema =
    current.name == schema.name && current.id /= schema.id


saveSchema : List Schema -> Maybe Schema -> List Schema
saveSchema schemas maybeSchema =
    case maybeSchema of
        Just schema ->
            List.map
                (\s ->
                    if schema.id == s.id then
                        schema
                    else
                        s
                )
                schemas

        Nothing ->
            schemas



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ title "My Schemas"
        , content model
        ]


title : String -> Html msg
title title =
    div [] [ h2 [] [ text title ] ]


content : Model -> Html Msg
content model =
    case model.error of
        Just message ->
            div []
                [ errorMessage message
                , createSchemaInput model.schemaNameInput
                , createSchemaButton
                , schemaList model.schemas model.editingSchema
                ]

        Nothing ->
            div []
                [ createSchemaInput model.schemaNameInput
                , createSchemaButton
                , schemaList model.schemas model.editingSchema
                ]


errorMessage : String -> Html Msg
errorMessage message =
    aside [] [ p [] [ text message ] ]


createSchemaInput : String -> Html Msg
createSchemaInput name =
    input [ value name, onInput InputSchemaName ] []


createSchemaButton : Html Msg
createSchemaButton =
    button [ onClick ValidateNewSchema ] [ text "Add Schema" ]


schemaList : List Schema -> Maybe Schema -> Html Msg
schemaList schemas editingSchema =
    case editingSchema of
        Just schema ->
            ul [] (List.map (renderSchema schema) schemas)

        Nothing ->
            ul [] (List.map (schemaView True) schemas)


renderSchema : Schema -> Schema -> Html Msg
renderSchema editingSchema schema =
    if schema.id == editingSchema.id then
        editSchemaView editingSchema
    else
        schemaView False schema


schemaLink : Schema -> Html Msg
schemaLink { id, name } =
    Router.link Goto (Router.Schema id) [] [ text name ]


schemaView : Bool -> Schema -> Html Msg
schemaView showButtons schema =
    if showButtons then
        li [] [ schemaLink schema, editSchemaButton schema.id, deleteSchmeaButton schema.id ]
    else
        li [] [ text schema.name ]


editSchemaView : Schema -> Html Msg
editSchemaView schema =
    div []
        [ input [ value schema.name, onInput InputEditingSchemaName ] []
        , button [ onClick ValidateUpdatedSchema ] [ text "Save" ]
        ]


editSchemaButton : Int -> Html Msg
editSchemaButton id =
    button [ onClick (EditSchema id) ] [ text "Edit Name" ]


deleteSchmeaButton : Int -> Html Msg
deleteSchmeaButton id =
    button [ onClick (DeleteSchema id) ] [ text "Delete" ]
