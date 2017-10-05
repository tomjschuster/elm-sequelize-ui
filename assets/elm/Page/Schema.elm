module Page.Schema
    exposing
        ( Model
        , Msg
        , init
        , initialModel
        , subscriptions
        , update
        , view
        )

import AppUpdate exposing (AppUpdate)
import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Data.Combined exposing (SchemaWithEntities)
import Data.Entity exposing (Entity)
import Data.Schema as Schema exposing (Schema)
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , h2
        , h3
        , input
        , li
        , main_
        , section
        , text
        , ul
        )
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Http
import Request.Entity as RE
import Request.Schema as RS
import Router exposing (Route)
import Utils.Handlers exposing (customOnKeyDown, onEnter, onEscape)
import Utils.Keys exposing (Key(..))
import Views.Breadcrumbs as BC
import Views.ChangesetError as CE


-- MODEL


type alias Model =
    { schema : Schema
    , entities : List Entity
    , editingName : Maybe String
    , newEntityInput : String
    , editingEntity : Maybe Entity
    , toDeleteId : Maybe Int
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    Model Schema.empty [] Nothing "" Nothing Nothing []


init : Int -> Cmd Msg
init id =
    RS.oneWithEntities id |> Http.send LoadSchemaWithEntities



-- UPDATE


type Msg
    = Goto Route
      -- SCHEMA
    | LoadSchemaWithEntities (Result Http.Error SchemaWithEntities)
    | LoadSchema (Result Http.Error Schema)
    | EditSchemaName
    | InputSchemaName String
    | CancelEditSchemaName
    | SaveSchemaName
    | Destroy
    | RemoveSchema (Result Http.Error ())
      -- ENTITIES
    | InputNewEntityName String
    | CreateEntity
    | LoadEntity (Result Http.Error Entity)
    | EditEntityName Int
    | InputEditingEntityName String
    | CancelEditEntityName
    | SaveEntityName
    | UpdateEntity (Result Http.Error Entity)
    | DestroyEntity Int
    | RemoveEntity (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg, AppUpdate )
update msg model =
    case msg of
        Goto route ->
            ( model
            , Router.goto route
            , AppUpdate.none
            )

        -- SCHEMA
        LoadSchemaWithEntities (Ok { schema, entities }) ->
            ( { model
                | schema = schema
                , entities = entities
                , editingName = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadSchemaWithEntities (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        LoadSchema (Ok schema) ->
            ( { model
                | schema = schema
                , editingName = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadSchema (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        EditSchemaName ->
            ( { model | editingName = Just model.schema.name }
            , Cmd.none
            , AppUpdate.none
            )

        InputSchemaName name ->
            ( { model | editingName = Just name }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditSchemaName ->
            ( { model | editingName = Nothing }
            , Cmd.none
            , AppUpdate.none
            )

        SaveSchemaName ->
            ( model
            , model.editingName
                |> saveSchemaName model.schema
                |> RS.update
                |> Http.send LoadSchema
            , AppUpdate.none
            )

        Destroy ->
            ( model
            , RS.destroy model.schema.id |> Http.send RemoveSchema
            , AppUpdate.none
            )

        RemoveSchema (Ok ()) ->
            ( model
            , Router.goto Router.Home
            , AppUpdate.none
            )

        RemoveSchema (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        -- ENTITIES
        InputNewEntityName newEntityInput ->
            ( { model | newEntityInput = newEntityInput }
            , Cmd.none
            , AppUpdate.none
            )

        CreateEntity ->
            ( model
            , RE.create model.newEntityInput model.schema.id
                |> Http.send LoadEntity
            , AppUpdate.none
            )

        LoadEntity (Ok entity) ->
            ( { model
                | entities = model.entities ++ [ entity ]
                , newEntityInput = ""
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadEntity (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        EditEntityName id ->
            ( { model
                | editingEntity = getEditingEntity id model.entities
              }
            , Cmd.none
            , AppUpdate.none
            )

        InputEditingEntityName name ->
            ( { model
                | editingEntity =
                    Maybe.map (updateEntityName name) model.editingEntity
              }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditEntityName ->
            ( { model | editingEntity = Nothing }
            , Cmd.none
            , AppUpdate.none
            )

        SaveEntityName ->
            ( model
            , model.editingEntity
                |> Maybe.map (RE.update >> Http.send UpdateEntity)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        UpdateEntity (Ok entity) ->
            ( { model
                | entities = List.map (replaceEntity entity) model.entities
                , editingEntity = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateEntity (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        DestroyEntity id ->
            ( { model | toDeleteId = Just id }
            , RE.destroy id
                |> Http.send RemoveEntity
            , AppUpdate.none
            )

        RemoveEntity (Ok ()) ->
            ( { model
                | entities =
                    List.filter
                        (.id >> Just >> (/=) model.toDeleteId)
                        model.entities
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        RemoveEntity (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )



-- SCHEMA NAME UPDATE


saveSchemaName : Schema -> Maybe String -> Schema
saveSchemaName schema editingName =
    editingName
        |> Maybe.map (updateSchemaName schema)
        |> Maybe.withDefault schema


updateSchemaName : Schema -> String -> Schema
updateSchemaName schema name =
    { schema | name = name }



-- ENTITIES UPDATE


getEditingEntity : Int -> List Entity -> Maybe Entity
getEditingEntity id =
    List.filter (.id >> (==) id) >> List.head


updateEntityName : String -> Entity -> Entity
updateEntityName name entity =
    { entity | name = name }


replaceEntity : Entity -> Entity -> Entity
replaceEntity newEntity entity =
    if entity.id == newEntity.id then
        newEntity
    else
        entity



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ breadCrumbs model.schema
        , title model.editingName model.schema.name
        , content model
        ]


breadCrumbs : Schema -> Html Msg
breadCrumbs schema =
    BC.view Goto
        [ BC.home, BC.schema schema ]



-- NAME VIEW


title : Maybe String -> String -> Html Msg
title editingName name =
    section [] (nameChildren editingName name)


nameChildren : Maybe String -> String -> List (Html Msg)
nameChildren maybeEditingName name =
    maybeEditingName
        |> Maybe.map editingNameChildren
        |> Maybe.withDefault (normalNameChildren name)


editingNameChildren : String -> List (Html Msg)
editingNameChildren name =
    [ editSchemaNameInput name
    , cancelEditSchemaNameButton
    , saveSchemaNameButton
    ]


normalNameChildren : String -> List (Html Msg)
normalNameChildren name =
    [ schemaName name
    , editSchemaNameButton
    , deleteSchemaButton
    ]


schemaName : String -> Html Msg
schemaName name =
    h2 [] [ text name ]


editSchemaNameButton : Html Msg
editSchemaNameButton =
    button [ onClick EditSchemaName ] [ text "Edit Name" ]


deleteSchemaButton : Html Msg
deleteSchemaButton =
    button [ onClick Destroy ] [ text "Delete Schema" ]


editSchemaNameInput : String -> Html Msg
editSchemaNameInput name =
    input
        [ value name
        , onInput InputSchemaName
        , onEnter SaveSchemaName
        , onEscape CancelEditSchemaName
        ]
        []


cancelEditSchemaNameButton : Html Msg
cancelEditSchemaNameButton =
    button [ onClick CancelEditSchemaName ] [ text "Cancel" ]


saveSchemaNameButton : Html Msg
saveSchemaNameButton =
    button [ onClick SaveSchemaName ] [ text "Save" ]


content : Model -> Html Msg
content model =
    section [] (contentChildren model)



-- ENTITIES VIEW


contentChildren : Model -> List (Html Msg)
contentChildren { editingEntity, entities, newEntityInput, errors } =
    CE.prependIfErrors
        errors
        [ entitiesTitle
        , createEntityView newEntityInput
        , entitiesListView editingEntity entities
        ]


entitiesTitle : Html msg
entitiesTitle =
    h3 [] [ text "Models" ]


createEntityView : String -> Html Msg
createEntityView newEntityInput =
    div []
        [ input
            [ value newEntityInput
            , onInput InputNewEntityName
            , onEnter CreateEntity
            ]
            []
        , button [ onClick CreateEntity ] [ text "Create Model" ]
        ]


entitiesListView : Maybe Entity -> List Entity -> Html Msg
entitiesListView editingEntity entities =
    ul [] (List.map (entityView editingEntity) entities)


entityView : Maybe Entity -> Entity -> Html Msg
entityView editingEntity entity =
    li [] (entityChildrenView entity editingEntity)


entityChildrenView : Entity -> Maybe Entity -> List (Html Msg)
entityChildrenView entity =
    Maybe.map (getEditingEntityChildren entity)
        >> Maybe.withDefault (normalEntityChildren entity)


getEditingEntityChildren : Entity -> Entity -> List (Html Msg)
getEditingEntityChildren entity editingEntity =
    if editingEntity.id == entity.id then
        editingEntityChildren editingEntity
    else
        [ text entity.name ]


normalEntityChildren : Entity -> List (Html Msg)
normalEntityChildren entity =
    [ entityLink entity
    , editEntityNameButton entity.id
    , deleteEntityButton entity.id
    ]


editingEntityChildren : Entity -> List (Html Msg)
editingEntityChildren { name } =
    [ editEntityNameInput name
    , cancelEditEntityNameButton
    , saveEntityNameButton
    ]


entityLink : Entity -> Html Msg
entityLink { id, name, schemaId } =
    Router.link Goto (Router.Entity schemaId id) [] [ text name ]


editEntityNameButton : Int -> Html Msg
editEntityNameButton id =
    button [ onClick (EditEntityName id) ] [ text "Edit Name" ]


deleteEntityButton : Int -> Html Msg
deleteEntityButton id =
    button [ onClick (DestroyEntity id) ] [ text "Delete" ]


editEntityNameInput : String -> Html Msg
editEntityNameInput name =
    input
        [ value name
        , onInput InputEditingEntityName
        , customOnKeyDown onEntityNameKeyDown
        ]
        []


onEntityNameKeyDown : Key -> Maybe Msg
onEntityNameKeyDown key =
    case key of
        Enter ->
            Just SaveEntityName

        Escape ->
            Just CancelEditEntityName

        _ ->
            Nothing


cancelEditEntityNameButton : Html Msg
cancelEditEntityNameButton =
    button [ onClick CancelEditEntityName ] [ text "Cancel" ]


saveEntityNameButton : Html Msg
saveEntityNameButton =
    button [ onClick SaveEntityName ] [ text "Save" ]
