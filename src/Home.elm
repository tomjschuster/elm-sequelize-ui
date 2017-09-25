module Home exposing (Model, Msg, init, initialModel, subscriptions, update, view)

import Data exposing (Schema, emptySchema, schemaDecoder)
import Html exposing (Html, aside, button, div, h2, input, li, main_, p, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Http
import Request
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
    Http.send LoadSchemas Request.getSchemas



-- UPDATE


type Msg
    = LoadSchemas (Result Http.Error (List Schema))
    | CreateSchema (Result String String)
    | LoadNewSchema (Result Http.Error Schema)
    | InputSchemaName String
    | ValidateSchema
    | EditSchema Int
    | InputEditingSchemaName String
    | SaveSchema
    | DeleteSchema Int
    | RemoveSchema (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateSchema (Ok name) ->
            ( { model | schemaNameInput = "" }, Request.createSchema model.schemaNameInput |> Http.send LoadNewSchema )

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

        DeleteSchema id ->
            ( { model | toDeleteId = Just id }, Request.deleteSchema id |> Http.send RemoveSchema )

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

        ValidateSchema ->
            ( model, validateSchema model.schemas model.schemaNameInput |> Task.attempt CreateSchema )

        EditSchema id ->
            let
                schema =
                    model.schemas |> List.filter (.id >> (==) id) |> List.head
            in
            ( { model | editingSchema = schema }, Cmd.none )

        InputEditingSchemaName name ->
            ( { model | editingSchema = Maybe.map (\s -> { s | name = name }) model.editingSchema }, Cmd.none )

        SaveSchema ->
            ( { model
                | schemas = saveSchema model.schemas model.editingSchema
                , editingSchema = Nothing
              }
            , Cmd.none
            )


validateSchema : List Schema -> String -> Task String String
validateSchema schemas name =
    Task.succeed name
        |> Task.andThen validateNameExists
        |> Task.andThen (validateNameNotTaken schemas)


validateNameExists : String -> Task String String
validateNameExists name =
    if name == "" then
        Task.fail "Please enter a name"
    else
        Task.succeed name


validateNameNotTaken : List Schema -> String -> Task String String
validateNameNotTaken schemas name =
    if schemas |> List.map .name |> List.member name then
        Task.fail ("Schema named '" ++ name ++ "' alread exists.")
    else
        Task.succeed name


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
    button [ onClick ValidateSchema ] [ text "Add Schema" ]


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


schemaView : Bool -> Schema -> Html Msg
schemaView hideButtons schema =
    if hideButtons then
        li [] [ text schema.name, editSchemaButton schema.id, deleteSchmeaButton schema.id ]
    else
        li [] [ text schema.name ]


editSchemaView : Schema -> Html Msg
editSchemaView schema =
    div []
        [ input [ value schema.name, onInput InputEditingSchemaName ] []
        , button [ onClick SaveSchema ] [ text "Save" ]
        ]


editSchemaButton : Int -> Html Msg
editSchemaButton id =
    button [ onClick (EditSchema id) ] [ text "Edit" ]


deleteSchmeaButton : Int -> Html Msg
deleteSchmeaButton id =
    button [ onClick (DeleteSchema id) ] [ text "Delete" ]
