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
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, int, s, top)


main : Program Never Model Msg
main =
    Navigation.program (getRoute >> Goto)
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Route
    = HomeRoute
    | SchemaRoute Int
    | NotFound


type Page
    = HomePage HomeModel
    | SchemaPage SchemaModel


routeParser : Parser (Route -> Route) Route
routeParser =
    Url.oneOf
        [ Url.map HomeRoute top
        , Url.map HomeRoute (s "home")
        , Url.map SchemaRoute (s "schema" </> int)
        ]


getRoute : Location -> Route
getRoute =
    Url.parsePath routeParser >> Maybe.withDefault NotFound


routeToPage : Route -> Page
routeToPage route =
    case route of
        HomeRoute ->
            HomePage initialHomeModel

        SchemaRoute id ->
            SchemaPage initialSchemaModel

        NotFound ->
            HomePage initialHomeModel



-- MODEL


type alias Model =
    { page : Page
    , homeModel : HomeModel
    , singleSchemaModel : SchemaModel
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        page =
            getRoute location |> routeToPage
    in
    Model page initialHomeModel initialSchemaModel ! []



-- UPDATE


type Msg
    = Goto Route
    | HomeMsg HomeMsg
    | SchemaMsg SchemaMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Goto route ->
            ( { model | page = routeToPage route }, Cmd.none )

        HomeMsg homeMsg ->
            let
                ( homeModel, homeCmd ) =
                    updateHome homeMsg model.homeModel
            in
            ( { model | homeModel = homeModel }, Cmd.map HomeMsg homeCmd )

        SchemaMsg singleSchemaMsg ->
            let
                ( singleSchemaModel, singleSchemaCmd ) =
                    updateSchema singleSchemaMsg model.singleSchemaModel
            in
            ( { model | singleSchemaModel = singleSchemaModel }, Cmd.map SchemaMsg singleSchemaCmd )


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
        HomePage homeModel ->
            homePage homeModel |> Html.map HomeMsg

        SchemaPage schemaModel ->
            singleSchemaView schemaModel |> Html.map SchemaMsg


pageTitle : String -> Html msg
pageTitle title =
    div [] [ h2 [] [ text title ] ]



-- SCHEMAS
-- SCHEMAS MODEL


type alias HomeModel =
    { schemas : List Schema
    , schemaNameInput : String
    , editingSchema : Maybe Schema
    , error : Maybe String
    , nextId : Int
    }


initialHomeModel : HomeModel
initialHomeModel =
    HomeModel [] "" Nothing Nothing 1


type alias Schema =
    { id : Int
    , name : String
    }


emptySchema : Schema
emptySchema =
    Schema 0 ""



-- SCHEMAS UPDATE


type HomeMsg
    = InputSchemaName String
    | AddSchema
    | EditSchema Int
    | InputEditingSchemaName String
    | SaveSchema
    | DeleteSchema Int


updateHome : HomeMsg -> HomeModel -> ( HomeModel, Cmd HomeMsg )
updateHome msg model =
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


homePage : HomeModel -> Html HomeMsg
homePage model =
    main_ []
        [ pageTitle "My Schemas"
        , homeContent model
        ]


homeContent : HomeModel -> Html HomeMsg
homeContent model =
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


errorMessage : String -> Html HomeMsg
errorMessage message =
    aside [] [ p [] [ text message ] ]


createSchemaInput : String -> Html HomeMsg
createSchemaInput name =
    input [ value name, onInput InputSchemaName ] []


createSchemaButton : Html HomeMsg
createSchemaButton =
    button [ onClick AddSchema ] [ text "Add Schema" ]


schemaList : List Schema -> Maybe Schema -> Html HomeMsg
schemaList schemas editingSchema =
    case editingSchema of
        Just schema ->
            ul [] (List.map (renderSchema schema) schemas)

        Nothing ->
            ul [] (List.map (schemaView True) schemas)


renderSchema : Schema -> Schema -> Html HomeMsg
renderSchema editingSchema schema =
    if schema.id == editingSchema.id then
        editSchemaView editingSchema
    else
        schemaView False schema


schemaView : Bool -> Schema -> Html HomeMsg
schemaView hideButtons schema =
    if hideButtons then
        li [] [ text schema.name, editSchemaButton schema.id, deleteSchmeaButton schema.id ]
    else
        li [] [ text schema.name ]


editSchemaView : Schema -> Html HomeMsg
editSchemaView schema =
    div []
        [ input [ value schema.name, onInput InputEditingSchemaName ] []
        , button [ onClick SaveSchema ] [ text "Save" ]
        ]


editSchemaButton : Int -> Html HomeMsg
editSchemaButton id =
    button [ onClick (EditSchema id) ] [ text "Edit" ]


deleteSchmeaButton : Int -> Html HomeMsg
deleteSchmeaButton id =
    button [ onClick (DeleteSchema id) ] [ text "Delete" ]



-- SINGLE SCHEMA
-- SINGLE SCHEMA MODEL


type alias SchemaModel =
    { schema : Schema }


initialSchemaModel : SchemaModel
initialSchemaModel =
    SchemaModel emptySchema


initSchema : Schema -> SchemaModel
initSchema schema =
    SchemaModel schema



-- SINGLE SCHEMA UPDATE


type SchemaMsg
    = NoOp


updateSchema : SchemaMsg -> SchemaModel -> ( SchemaModel, Cmd SchemaMsg )
updateSchema msg model =
    model ! []



-- SINGLE SCHEMA VIEW


singleSchemaView : SchemaModel -> Html SchemaMsg
singleSchemaView model =
    div [] [ h1 [] [ text model.schema.name ] ]
