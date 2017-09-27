module Page.Schema exposing (Model, Msg, init, initialModel, subscriptions, update, view)

import Data.Entity exposing (Entity)
import Data.Schema exposing (Schema, emptySchema)
import Html exposing (Html, a, button, div, h2, h3, input, li, section, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Http
import Request.Entity as RE
import Request.Schema as RS
import Router


-- MODEL


type alias Model =
    { schema : Schema
    , editingName : Maybe String
    , newEntityInput : String
    , toDeleteId : Maybe Int
    , error : Maybe String
    }


initialModel : Model
initialModel =
    Model emptySchema Nothing "" Nothing Nothing


init : Int -> Cmd Msg
init id =
    RS.one id |> Http.send LoadSchema



-- UPDATE


type Msg
    = LoadSchema (Result Http.Error Schema)
    | RemoveSchema (Result Http.Error ())
    | LoadEntity (Result Http.Error Entity)
    | RemoveEntity (Result Http.Error ())
    | EditName
    | UpdateEditingName String
    | CancelEditName
    | SaveName
    | Destroy
    | UpdateNewEntityInput String
    | CreateEntity
    | DestroyEntity Int


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

        LoadEntity (Ok entity) ->
            ( { model
                | schema = addEntity model.schema entity
                , newEntityInput = ""
                , error = Nothing
              }
            , Cmd.none
            )

        LoadEntity (Err error) ->
            ( { model | error = Just "Error creating entity" }, Cmd.none )

        RemoveEntity (Ok ()) ->
            ( { model
                | schema = removeEntity model.schema model.toDeleteId
                , error = Nothing
              }
            , Cmd.none
            )

        RemoveEntity (Err error) ->
            ( { model | error = Just "Error deleting model" }, Cmd.none )

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

        UpdateNewEntityInput newEntityInput ->
            ( { model | newEntityInput = newEntityInput }, Cmd.none )

        CreateEntity ->
            ( model, RE.create model.newEntityInput model.schema.id |> Http.send LoadEntity )

        DestroyEntity id ->
            ( { model | toDeleteId = Just id }, RE.destroy id |> Http.send RemoveEntity )


saveSchemaName : Schema -> Maybe String -> Schema
saveSchemaName schema editingName =
    editingName
        |> Maybe.map (updateSchemaName schema)
        |> Maybe.withDefault schema


updateSchemaName : Schema -> String -> Schema
updateSchemaName schema name =
    { schema | name = name }


addEntity : Schema -> Entity -> Schema
addEntity schema entity =
    { schema | entities = schema.entities ++ [ entity ] }


removeEntity : Schema -> Maybe Int -> Schema
removeEntity schema maybeId =
    { schema | entities = List.filter (.id >> Just >> (/=) maybeId) schema.entities }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view { schema, editingName, newEntityInput } =
    div []
        [ nameView editingName schema.name
        , entitiesView schema.entities newEntityInput
        ]



-- NAME VIEW


nameView : Maybe String -> String -> Html Msg
nameView editingName name =
    section [] (nameChildren editingName name)


nameChildren : Maybe String -> String -> List (Html Msg)
nameChildren maybeEditingName name =
    case maybeEditingName of
        Just editingName ->
            [ editNameInput editingName, cancelEditNameButton, saveNameButton ]

        Nothing ->
            [ schemaName name, editNameButton, deleteSchemaButton ]


schemaName : String -> Html Msg
schemaName name =
    h2 [] [ text name ]


editNameButton : Html Msg
editNameButton =
    button [ onClick EditName ] [ text "Edit Title" ]


deleteSchemaButton : Html Msg
deleteSchemaButton =
    button [ onClick Destroy ] [ text "Delete Schema" ]


editNameInput : String -> Html Msg
editNameInput name =
    input [ value name, onInput UpdateEditingName ] []


cancelEditNameButton : Html Msg
cancelEditNameButton =
    button [ onClick CancelEditName ] [ text "Cancel" ]


saveNameButton : Html Msg
saveNameButton =
    button [ onClick SaveName ] [ text "Save" ]



-- ENTITIES VIEW


entitiesView : List Entity -> String -> Html Msg
entitiesView entities newEntityInput =
    section []
        [ h3 [] [ text "Models" ]
        , createEntityView newEntityInput
        , entitiesListView entities
        ]


createEntityView : String -> Html Msg
createEntityView newEntityInput =
    div []
        [ input [ value newEntityInput, onInput UpdateNewEntityInput ] []
        , button [ onClick CreateEntity ] [ text "Create Model" ]
        ]


entitiesListView : List Entity -> Html Msg
entitiesListView entities =
    ul [] (List.map entityView entities)


entityView : Entity -> Html Msg
entityView { id, name } =
    li [] [ text name, deleteEntityButton id ]


deleteEntityButton : Int -> Html Msg
deleteEntityButton id =
    button [ onClick (DestroyEntity id) ] [ text "Delete" ]
