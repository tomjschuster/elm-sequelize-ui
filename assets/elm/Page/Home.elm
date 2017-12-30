module Page.Home exposing (Model, Msg, init, initialModel, subscriptions, update, view)

import AppUpdate exposing (AppUpdate)
import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Data.Schema as Schema exposing (Schema)
import Dom
import Html
    exposing
        ( Html
        , button
        , div
        , h2
        , input
        , li
        , main_
        , text
        , ul
        )
import Html.Attributes as Attr
import Html.Events as Evt
import Http
import Request.Schema as SchemaReq
import Router exposing (Route)
import Task
import Utils.Events as EvtUtils
import Utils.Keys exposing (Key(..))
import Views.Breadcrumbs as BreadCrumbs
import Views.ChangesetError as CE


-- MODEL


type alias Model =
    { schemas : List Schema
    , newSchema : Schema
    , editingSchema : Maybe Schema
    , toDeleteId : Maybe Int
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    Model [] Schema.empty Nothing Nothing []


init : ( Model, Cmd Msg )
init =
    ( initialModel, Http.send LoadSchemas SchemaReq.index )



-- UPDATE


type Msg
    = NoOp
    | FocuseResult (Result Dom.Error ())
    | Goto Route
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
        NoOp ->
            ( model, Cmd.none, AppUpdate.none )

        FocuseResult (Ok ()) ->
            ( model, Cmd.none, AppUpdate.none )

        FocuseResult (Err _) ->
            ( model, Cmd.none, AppUpdate.none )

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
            ( { model | newSchema = Schema.updateName name model.newSchema }
            , Cmd.none
            , AppUpdate.none
            )

        CreateSchema ->
            ( { model | editingSchema = Nothing }
            , SchemaReq.create model.newSchema |> Http.send LoadNewSchema
            , AppUpdate.none
            )

        LoadNewSchema (Ok schema) ->
            ( { model
                | newSchema = Schema.empty
                , schemas = model.schemas ++ [ schema ]
                , errors = []
              }
            , Dom.focus "create-schema" |> Task.attempt FocuseResult
            , AppUpdate.none
            )

        LoadNewSchema (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Dom.focus "create-schema" |> Task.attempt FocuseResult
            , AppUpdate.none
            )

        -- EDIT SCHEMA
        EditSchema id ->
            ( { model
                | editingSchema =
                    model.schemas |> List.filter (.id >> (==) id) |> List.head
                , errors = []
              }
            , Dom.focus "edit-schema-name" |> Task.attempt FocuseResult
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
            ( { model | editingSchema = Nothing, errors = [] }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateSchema ->
            case model.editingSchema of
                Just schema ->
                    ( { model | errors = [] }
                    , SchemaReq.update schema |> Http.send LoadUpdatedSchema
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
            , Dom.focus "edit-schema-name" |> Task.attempt FocuseResult
            , AppUpdate.none
            )

        -- DELETE SCHEMA
        DestroySchema id ->
            ( { model | editingSchema = Nothing, toDeleteId = Just id }
            , SchemaReq.destroy id |> Http.send RemoveSchema
            , AppUpdate.none
            )

        RemoveSchema (Ok ()) ->
            ( { model
                | schemas =
                    List.filter
                        (.id >> Just >> (/=) model.toDeleteId)
                        model.schemas
                , toDeleteId = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        RemoveSchema (Err error) ->
            ( { model
                | errors = ChangesetError.parseHttpError error
                , toDeleteId = Nothing
              }
            , Cmd.none
            , AppUpdate.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ breadCrumbs
        , title
        , schemasView model
        ]


title : Html msg
title =
    h2 [] [ text "My Schemas" ]


breadCrumbs : Html Msg
breadCrumbs =
    BreadCrumbs.view Goto [ BreadCrumbs.home ]


schemasView : Model -> Html Msg
schemasView model =
    div [] (CE.prependIfErrors model.errors (schemaChildren model))


schemaChildren : Model -> List (Html Msg)
schemaChildren model =
    [ createSchemaInput model.newSchema.name
    , createSchemaButton
    , schemaList model.schemas model.editingSchema
    ]



-- CREATE SCHEMA VIEW


createSchemaInput : String -> Html Msg
createSchemaInput name =
    input
        [ Attr.id "create-schema"
        , Attr.value name
        , Evt.onInput InputSchemaName
        , EvtUtils.onEnter CreateSchema
        ]
        []


createSchemaButton : Html Msg
createSchemaButton =
    button [ Evt.onClick CreateSchema ] [ text "Add Schema" ]



-- LIST SCHEMAS


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



-- READ SCHEMAS


normalSchemaListItemChildren : Schema -> List (Html Msg)
normalSchemaListItemChildren schema =
    [ schemaLink schema
    , editSchemaNameButton schema.id
    , deleteSchemaButton schema.id
    ]


schemaLink : Schema -> Html Msg
schemaLink { id, name } =
    Router.link Goto (Router.Schema id) [] [ text name ]


editSchemaNameButton : Int -> Html Msg
editSchemaNameButton id =
    button [ Evt.onClick (EditSchema id) ] [ text "Edit Name" ]


deleteSchemaButton : Int -> Html Msg
deleteSchemaButton id =
    button [ Evt.onClick (DestroySchema id) ] [ text "Delete" ]



-- UPDATE SCHEMAS


editingSchemaListItemChildren : Schema -> Schema -> List (Html Msg)
editingSchemaListItemChildren schema editingSchema =
    if editingSchema.id == schema.id then
        [ editSchemaNameInput editingSchema.name
        , cancelEditSchemaNameButton
        , saveSchemaNameButton
        ]
    else
        [ text schema.name ]


editSchemaNameInput : String -> Html Msg
editSchemaNameInput name =
    input
        [ Attr.id "edit-schema-name"
        , Attr.value name
        , Evt.onInput InputEditingSchemaName
        , EvtUtils.customOnKeyDown onSchemaNameKeyDown
        ]
        []


onSchemaNameKeyDown : Key -> Maybe Msg
onSchemaNameKeyDown key =
    case key of
        Enter ->
            Just UpdateSchema

        Escape ->
            Just CancelEditSchemaName

        _ ->
            Nothing


cancelEditSchemaNameButton : Html Msg
cancelEditSchemaNameButton =
    button [ Evt.onClick CancelEditSchemaName ] [ text "Cancel" ]


saveSchemaNameButton : Html Msg
saveSchemaNameButton =
    button [ Evt.onClick UpdateSchema ] [ text "Save" ]
