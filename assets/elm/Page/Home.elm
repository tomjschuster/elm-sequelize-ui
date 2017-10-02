module Page.Home exposing (Model, Msg, init, initialModel, subscriptions, update, view)

import Data.Schema as Schema exposing (Schema)
import Html exposing (Html, a, aside, button, div, h2, input, li, main_, p, text, ul)
import Html.Attributes exposing (href, value)
import Html.Events as Events exposing (onClick, onInput)
import Http
import Request.Schema as RS
import Router exposing (Route)
import Task exposing (Task)
import Views.Breadcrumbs as BC


-- MODEL


type alias Model =
    { schemas : List Schema
    , schemaNameInput : String
    , editingSchema : Maybe Schema
    , toDeleteId : Maybe Int
    , error : Maybe String
    }


initialModel : Model
initialModel =
    Model [] "" Nothing Nothing Nothing


init : Cmd Msg
init =
    Http.send LoadSchemas RS.all



-- UPDATE


type Msg
    = Goto Route
    | LoadSchemas (Result Http.Error (List Schema))
      -- CREATE SCHEMA
    | InputSchemaName String
    | ValidateNewSchema
    | CreateSchema (Result String Schema)
    | LoadNewSchema (Result Http.Error Schema)
      -- EDIT SCHEMA
    | EditSchema Int
    | InputEditingSchemaName String
    | CancelEditSchemaName
    | ValidateUpdatedSchema
    | UpdateSchema (Result String Schema)
    | LoadUpdatedSchema (Result Http.Error Schema)
      -- DELETE SCHEMA
    | DestroySchema Int
    | RemoveSchema (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Goto route ->
            ( model, Router.goto route )

        LoadSchemas (Ok schemas) ->
            ( { model | schemas = schemas, error = Nothing }, Cmd.none )

        LoadSchemas (Err error) ->
            let
                x =
                    Debug.log "a" error
            in
            ( { model | error = Just "Error loading schemas" }, Cmd.none )

        -- CREATE SCHEMA
        InputSchemaName name ->
            ( { model | schemaNameInput = name }, Cmd.none )

        ValidateNewSchema ->
            ( model
            , validateSchema model.schemas (draftSchema model.schemaNameInput)
                |> Task.attempt CreateSchema
            )

        CreateSchema (Ok _) ->
            ( { model | schemaNameInput = "" }
            , RS.create model.schemaNameInput |> Http.send LoadNewSchema
            )

        CreateSchema (Err error) ->
            ( { model | error = Just error }, Cmd.none )

        LoadNewSchema (Ok schema) ->
            ( { model
                | schemas = model.schemas ++ [ schema ]
                , error = Nothing
              }
            , Cmd.none
            )

        LoadNewSchema (Err error) ->
            ( { model | error = Just "Error creating schema" }, Cmd.none )

        -- EDIT SCHEMA
        EditSchema id ->
            let
                schema =
                    model.schemas |> List.filter (.id >> (==) id) |> List.head
            in
            ( { model | editingSchema = schema }, Cmd.none )

        InputEditingSchemaName name ->
            ( { model
                | editingSchema =
                    Maybe.map (\s -> { s | name = name }) model.editingSchema
              }
            , Cmd.none
            )

        CancelEditSchemaName ->
            ( { model | editingSchema = Nothing }, Cmd.none )

        ValidateUpdatedSchema ->
            ( model
            , model.editingSchema
                |> Maybe.map
                    (validateSchema model.schemas >> Task.attempt UpdateSchema)
                |> Maybe.withDefault Cmd.none
            )

        UpdateSchema (Ok _) ->
            case model.editingSchema of
                Just schema ->
                    ( { model | error = Nothing }
                    , RS.update schema |> Http.send LoadUpdatedSchema
                    )

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

        -- DELETE SCHEMA
        DestroySchema id ->
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


emptySchema : Schema
emptySchema =
    Schema.empty


draftSchema : String -> Schema
draftSchema name =
    { emptySchema | name = name }



-- SCHEMA NAME VALIDATION


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ breadCrumbs
        , title
        , content model
        ]


title : Html msg
title =
    h2 [] [ text "My Schemas" ]


breadCrumbs : Html Msg
breadCrumbs =
    BC.view Goto [ BC.home ]


content : Model -> Html Msg
content model =
    div [] (contentChildrenView model)


contentChildrenView : Model -> List (Html Msg)
contentChildrenView model =
    model.error
        |> Maybe.map (errorContentChildren model)
        |> Maybe.withDefault (normalContentChildren model)


errorContentChildren : Model -> String -> List (Html Msg)
errorContentChildren model =
    errorMessage >> flip (::) (normalContentChildren model)


normalContentChildren : Model -> List (Html Msg)
normalContentChildren model =
    [ createSchemaInput model.schemaNameInput
    , createSchemaButton
    , schemaList model.schemas model.editingSchema
    ]


errorMessage : String -> Html Msg
errorMessage message =
    aside [] [ p [] [ text message ] ]



-- CREATE SCHEMA VIEW


createSchemaInput : String -> Html Msg
createSchemaInput name =
    input [ value name, onInput InputSchemaName ] []


createSchemaButton : Html Msg
createSchemaButton =
    button [ onClick ValidateNewSchema ] [ text "Add Schema" ]



-- SCHEMAS VIEW


schemaList : List Schema -> Maybe Schema -> Html Msg
schemaList schemas editingSchema =
    ul [] (List.map (schemaListItem editingSchema) schemas)


schemaListItem : Maybe Schema -> Schema -> Html Msg
schemaListItem editingSchema schema =
    li [] (schemaListItemChildren editingSchema schema)


schemaListItemChildren : Maybe Schema -> Schema -> List (Html Msg)
schemaListItemChildren editingSchema schema =
    editingSchema
        |> Maybe.map (editingSchemaListItemChildren schema)
        |> Maybe.withDefault (normalSchemaListItemChildren schema)


editingSchemaListItemChildren : Schema -> Schema -> List (Html Msg)
editingSchemaListItemChildren schema editingSchema =
    if editingSchema.id == schema.id then
        [ editSchemaNameInput editingSchema.name
        , cancelEditSchemaNameButton
        , saveSchemaNameButton
        ]
    else
        [ text schema.name ]


normalSchemaListItemChildren : Schema -> List (Html Msg)
normalSchemaListItemChildren schema =
    [ schemaLink schema
    , editSchemaNameButton schema.id
    , deleteSchemaButton schema.id
    ]


schemaLink : Schema -> Html Msg
schemaLink { id, name } =
    Router.link Goto (Router.Schema id) [] [ text name ]


editSchemaNameInput : String -> Html Msg
editSchemaNameInput name =
    input [ value name, onInput InputEditingSchemaName ] []


cancelEditSchemaNameButton : Html Msg
cancelEditSchemaNameButton =
    button [ onClick CancelEditSchemaName ] [ text "Cancel" ]


saveSchemaNameButton : Html Msg
saveSchemaNameButton =
    button [ onClick ValidateUpdatedSchema ] [ text "Save" ]


editSchemaNameButton : Int -> Html Msg
editSchemaNameButton id =
    button [ onClick (EditSchema id) ] [ text "Edit Name" ]


deleteSchemaButton : Int -> Html Msg
deleteSchemaButton id =
    button [ onClick (DestroySchema id) ] [ text "Delete" ]
