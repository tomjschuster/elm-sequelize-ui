module Page.Home exposing (Model, Msg, init, initialModel, subscriptions, update, view)

import AppUpdate exposing (AppUpdate)
import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Data.Schema as Schema exposing (Schema)
import Html
    exposing
        ( Html
        , a
        , aside
        , button
        , div
        , h2
        , h3
        , input
        , li
        , main_
        , p
        , section
        , text
        , ul
        )
import Html.Attributes exposing (href, value)
import Html.Events as Events exposing (onClick, onInput)
import Http
import Request.Schema as RS
import Router exposing (Route)
import Task exposing (Task)
import Views.Breadcrumbs as BC
import Views.ChangesetError as CE


-- MODEL


type alias Model =
    { schemas : List Schema
    , schemaNameInput : String
    , editingSchema : Maybe Schema
    , toDeleteId : Maybe Int
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    Model [] "" Nothing Nothing []


init : Cmd Msg
init =
    Http.send LoadSchemas RS.all



-- UPDATE


type Msg
    = Goto Route
    | LoadSchemas (Result Http.Error (List Schema))
      -- CREATE SCHEMA
    | InputSchemaName String
    | CreateSchema
    | LoadNewSchema (Result Http.Error Schema)
      -- EDIT SCHEMA
    | EditSchema Int
    | InputEditingSchemaName String
    | CancelEditSchemaName
    | UpdateSchema
    | LoadUpdatedSchema (Result Http.Error Schema)
      -- DELETE SCHEMA
    | DestroySchema Int
    | RemoveSchema (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg, AppUpdate )
update msg model =
    case msg of
        Goto route ->
            ( model
            , Router.goto route
            , AppUpdate.none
            )

        LoadSchemas (Ok schemas) ->
            ( { model | schemas = schemas, errors = [] }
            , Cmd.none
            , AppUpdate.none
            )

        LoadSchemas (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        -- CREATE SCHEMA
        InputSchemaName name ->
            ( { model | schemaNameInput = name }
            , Cmd.none
            , AppUpdate.none
            )

        CreateSchema ->
            ( { model | schemaNameInput = "" }
            , RS.create model.schemaNameInput |> Http.send LoadNewSchema
            , AppUpdate.none
            )

        LoadNewSchema (Ok schema) ->
            ( { model
                | schemas = model.schemas ++ [ schema ]
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadNewSchema (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        -- EDIT SCHEMA
        EditSchema id ->
            ( { model
                | editingSchema =
                    model.schemas |> List.filter (.id >> (==) id) |> List.head
              }
            , Cmd.none
            , AppUpdate.none
            )

        InputEditingSchemaName name ->
            ( { model
                | editingSchema =
                    Maybe.map (\s -> { s | name = name }) model.editingSchema
              }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditSchemaName ->
            ( { model | editingSchema = Nothing }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateSchema ->
            case model.editingSchema of
                Just schema ->
                    ( { model | errors = [] }
                    , RS.update schema |> Http.send LoadUpdatedSchema
                    , AppUpdate.none
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    , AppUpdate.none
                    )

        LoadUpdatedSchema (Ok schema) ->
            ( { model
                | errors = []
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
            , AppUpdate.none
            )

        LoadUpdatedSchema (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        -- DELETE SCHEMA
        DestroySchema id ->
            ( { model | toDeleteId = Just id }
            , RS.destroy id |> Http.send RemoveSchema
            , AppUpdate.none
            )

        RemoveSchema (Ok ()) ->
            ( { model
                | schemas = List.filter (.id >> Just >> (/=) model.toDeleteId) model.schemas
                , toDeleteId = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        RemoveSchema (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error, toDeleteId = Nothing }
            , Cmd.none
            , AppUpdate.none
            )


emptySchema : Schema
emptySchema =
    Schema.empty


draftSchema : String -> Schema
draftSchema name =
    { emptySchema | name = name }



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
    div [] (contentChildren model)


contentChildren : Model -> List (Html Msg)
contentChildren model =
    CE.prependIfErrors model.errors (normalContentChildren model)


normalContentChildren : Model -> List (Html Msg)
normalContentChildren model =
    [ createSchemaInput model.schemaNameInput
    , createSchemaButton
    , schemaList model.schemas model.editingSchema
    ]



-- CREATE SCHEMA VIEW


createSchemaInput : String -> Html Msg
createSchemaInput name =
    input [ value name, onInput InputSchemaName ] []


createSchemaButton : Html Msg
createSchemaButton =
    button [ onClick CreateSchema ] [ text "Add Schema" ]



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
    button [ onClick UpdateSchema ] [ text "Save" ]


editSchemaNameButton : Int -> Html Msg
editSchemaNameButton id =
    button [ onClick (EditSchema id) ] [ text "Edit Name" ]


deleteSchemaButton : Int -> Html Msg
deleteSchemaButton id =
    button [ onClick (DestroySchema id) ] [ text "Delete" ]
