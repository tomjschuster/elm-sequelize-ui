module Main exposing (..)

import Html
    exposing
        ( Html
        , a
        , aside
        , button
        , div
        , footer
        , h1
        , h2
        , header
        , input
        , li
        , main_
        , nav
        , p
        , text
        , ul
        )
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { page : Page
    , schemasModel : SchemasModel
    , singleSchemaModel : SingleSchemaModel
    }


type Page
    = Schemas
    | SingleSchema


init : ( Model, Cmd Msg )
init =
    Model Schemas initialSchemasModel initialSingleSchemaModel ! []



-- UPDATE


type Msg
    = Goto Page
    | SchemasMsg SchemasMsg
    | SingleSchemaMsg SingleSchemaMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Goto page ->
            ( { model | page = page }, Cmd.none )

        SchemasMsg schemasMsg ->
            let
                ( schemasModel, schemasCmd ) =
                    updateSchemas schemasMsg model.schemasModel
            in
            ( { model | schemasModel = schemasModel }, Cmd.map SchemasMsg schemasCmd )

        SingleSchemaMsg singleSchemaMsg ->
            let
                ( singleSchemaModel, singleSchemaCmd ) =
                    updateSingleSchema singleSchemaMsg model.singleSchemaModel
            in
            ( { model | singleSchemaModel = singleSchemaModel }, Cmd.map SingleSchemaMsg singleSchemaCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ header []
            [ h1 [] [ text "Sequelize UI" ] ]
        , pageView model
        , footer []
            []
        ]


pageView : Model -> Html Msg
pageView model =
    case model.page of
        Schemas ->
            schemasPage model.schemasModel |> Html.map SchemasMsg

        SingleSchema ->
            singleSchemaView model.singleSchemaModel |> Html.map SingleSchemaMsg


pageTitle : String -> Html msg
pageTitle title =
    div [] [ h2 [] [ text title ] ]



-- SCHEMAS
-- SCHEMAS MODEL


type alias SchemasModel =
    { schemas : List Schema
    , schemaNameInput : String
    , editingSchema : Maybe Schema
    , error : Maybe String
    , nextId : Int
    }


initialSchemasModel : SchemasModel
initialSchemasModel =
    SchemasModel [] "" Nothing Nothing 1


type alias Schema =
    { id : Int
    , name : String
    }



-- SCHEMAS UPDATE


type SchemasMsg
    = InputSchemaName String
    | AddSchema
    | EditSchema Int
    | InputEditingSchemaName String
    | SaveSchema
    | DeleteSchema Int


updateSchemas : SchemasMsg -> SchemasModel -> ( SchemasModel, Cmd SchemasMsg )
updateSchemas msg model =
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



-- SCHEMAS VIEW


schemasPage : SchemasModel -> Html SchemasMsg
schemasPage model =
    main_ []
        [ pageTitle "Schemas"
        , schemasContent model
        ]


schemasContent : SchemasModel -> Html SchemasMsg
schemasContent model =
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


errorMessage : String -> Html SchemasMsg
errorMessage message =
    aside [] [ p [] [ text message ] ]


createSchemaInput : String -> Html SchemasMsg
createSchemaInput name =
    input [ value name, onInput InputSchemaName ] []


createSchemaButton : Html SchemasMsg
createSchemaButton =
    button [ onClick AddSchema ] [ text "Add Schema" ]


schemaList : List Schema -> Maybe Schema -> Html SchemasMsg
schemaList schemas editingSchema =
    case editingSchema of
        Just schema ->
            ul [] (List.map (renderSchema schema) schemas)

        Nothing ->
            ul [] (List.map (schemaView True) schemas)


renderSchema : Schema -> Schema -> Html SchemasMsg
renderSchema editingSchema schema =
    if schema.id == editingSchema.id then
        editSchemaView editingSchema
    else
        schemaView False schema


schemaView : Bool -> Schema -> Html SchemasMsg
schemaView hideButtons schema =
    if hideButtons then
        li [] [ text schema.name, editSchemaButton schema.id, deleteSchmeaButton schema.id ]
    else
        li [] [ text schema.name ]


editSchemaView : Schema -> Html SchemasMsg
editSchemaView schema =
    div []
        [ input [ value schema.name, onInput InputEditingSchemaName ] []
        , button [ onClick SaveSchema ] [ text "Save" ]
        ]


editSchemaButton : Int -> Html SchemasMsg
editSchemaButton id =
    button [ onClick (EditSchema id) ] [ text "Edit" ]


deleteSchmeaButton : Int -> Html SchemasMsg
deleteSchmeaButton id =
    button [ onClick (DeleteSchema id) ] [ text "Delete" ]



-- SINGLE SCHEMA
-- SINGLE SCHEMA MODEL


type alias SingleSchemaModel =
    {}


initialSingleSchemaModel : SingleSchemaModel
initialSingleSchemaModel =
    SingleSchemaModel



-- SINGLE SCHEMA UPDATE


type SingleSchemaMsg
    = NoOp


updateSingleSchema : SingleSchemaMsg -> SingleSchemaModel -> ( SingleSchemaModel, Cmd SingleSchemaMsg )
updateSingleSchema msg model =
    model ! []



-- SINGLE SCHEMA VIEW


singleSchemaView : SingleSchemaModel -> Html SingleSchemaMsg
singleSchemaView model =
    div [] []
