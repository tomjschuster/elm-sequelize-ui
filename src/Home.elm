module Home exposing (Model, Msg, initialModel, subscriptions, update, view)

import Data exposing (Schema, emptySchema)
import Html exposing (Html, aside, button, div, h2, input, li, main_, p, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)


-- MODEL


type alias Model =
    { schemas : List Schema
    , schemaNameInput : String
    , editingSchema : Maybe Schema
    , error : Maybe String
    , nextId : Int
    }


initialModel : Model
initialModel =
    Model [] "" Nothing Nothing 1



-- UPDATE


type Msg
    = InputSchemaName String
    | AddSchema
    | EditSchema Int
    | InputEditingSchemaName String
    | SaveSchema
    | DeleteSchema Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputSchemaName name ->
            ( { model | schemaNameInput = name }, Cmd.none )

        AddSchema ->
            if model.schemas |> List.filter (.name >> (==) model.schemaNameInput) |> List.head |> (==) Nothing then
                ( { model
                    | schemas = Schema model.nextId model.schemaNameInput :: model.schemas
                    , schemaNameInput = ""
                    , error = Nothing
                    , nextId = model.nextId + 1
                  }
                , Cmd.none
                )
            else
                ( { model | error = Just ("Schema named " ++ model.schemaNameInput ++ " already exists") }, Cmd.none )

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

        DeleteSchema id ->
            ( { model | schemas = List.filter (.id >> (/=) id) model.schemas }, Cmd.none )


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
    button [ onClick AddSchema ] [ text "Add Schema" ]


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
