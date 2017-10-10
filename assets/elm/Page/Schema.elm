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
import Data.Entity as Entity exposing (Entity)
import Data.Schema as Schema exposing (Schema)
import Dom
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
import Html.Attributes exposing (disabled, id, value)
import Html.Events exposing (onClick, onInput)
import Http
import Request.Entity as RE
import Request.Schema as RS
import Router exposing (Route)
import Task
import Utils.Handlers exposing (customOnKeyDown, onEnter)
import Utils.Keys exposing (Key(..))
import Views.Breadcrumbs as BC
import Views.ChangesetError as CE


-- MODEL


type alias Model =
    { schema : Schema
    , entities : List Entity
    , editingSchema : Maybe Schema
    , editing : Bool
    , newEntity : Entity
    , editingEntity : Maybe Entity
    , toDeleteId : Maybe Int
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    Model Schema.empty [] Nothing False Entity.empty Nothing Nothing []


init : Int -> ( Model, Cmd Msg )
init schemaId =
    ( { initialModel | newEntity = Entity.init schemaId }
    , RS.oneWithEntities schemaId |> Http.send LoadSchemaWithEntities
    )



-- UPDATE


type Msg
    = NoOp
    | FocusResult (Result Dom.Error ())
    | Goto Route
      -- SCHEMA
    | LoadSchemaWithEntities (Result Http.Error SchemaWithEntities)
    | LoadSchema (Result Http.Error Schema)
    | EditSchema
    | InputSchemaName String
    | CancelEditSchema
    | SaveSchema
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
        NoOp ->
            ( model, Cmd.none, AppUpdate.none )

        FocusResult (Ok ()) ->
            ( model, Cmd.none, AppUpdate.none )

        FocusResult (Err _) ->
            ( model, Cmd.none, AppUpdate.none )

        Goto route ->
            ( model
            , Router.goto route
            , AppUpdate.none
            )

        -- SCHEMA
        LoadSchemaWithEntities (Ok { schema, entities }) ->
            ( { model | schema = schema, entities = entities, errors = [] }
            , Cmd.none
            , AppUpdate.none
            )

        LoadSchemaWithEntities (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        LoadSchema (Ok schema) ->
            ( { model | schema = schema, editingSchema = Nothing, errors = [] }
            , Cmd.none
            , AppUpdate.none
            )

        LoadSchema (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        EditSchema ->
            ( { model
                | editingEntity = Nothing
                , editingSchema = Just model.schema
              }
            , Dom.focus "edit-schema-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        InputSchemaName name ->
            ( { model
                | editingSchema =
                    Maybe.map (Schema.updateName name) model.editingSchema
              }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditSchema ->
            ( { model | editingSchema = Nothing }
            , Cmd.none
            , AppUpdate.none
            )

        SaveSchema ->
            ( model
            , model.editingSchema
                |> Maybe.map (RS.update >> Http.send LoadSchema)
                |> Maybe.withDefault Cmd.none
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
        InputNewEntityName name ->
            ( { model | newEntity = Entity.updateName name model.newEntity }
            , Cmd.none
            , AppUpdate.none
            )

        CreateEntity ->
            ( model
            , RE.create model.newEntity
                |> Http.send LoadEntity
            , AppUpdate.none
            )

        LoadEntity (Ok entity) ->
            ( { model
                | entities = model.entities ++ [ entity ]
                , newEntity = Entity.init model.schema.id
                , errors = []
              }
            , Dom.focus "create-entity" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        LoadEntity (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Dom.focus "create-entity" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        EditEntityName id ->
            ( { model
                | editingSchema = Nothing
                , editingEntity =
                    model.entities |> List.filter (.id >> (==) id) >> List.head
              }
            , Dom.focus "edit-entity-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        InputEditingEntityName name ->
            ( { model
                | editingEntity =
                    Maybe.map (Entity.updateName name) model.editingEntity
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
                | entities =
                    List.map (Entity.replaceIfMatch entity) model.entities
                , editingEntity = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateEntity (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Dom.focus "edit-entity-name" |> Task.attempt FocusResult
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ breadCrumbs model.schema
        , schemaView model
        , entitiesView model
        ]


breadCrumbs : Schema -> Html Msg
breadCrumbs schema =
    BC.view Goto
        [ BC.home, BC.schema schema ]



-- SCHEMA VIEW


schemaView : Model -> Html Msg
schemaView { editingSchema, schema, editingEntity } =
    section [] (schemaChildren editingSchema schema)


schemaChildren : Maybe Schema -> Schema -> List (Html Msg)
schemaChildren editingSchema schema =
    editingSchema
        |> Maybe.map editingSchemaChildren
        |> Maybe.withDefault (readSchemaChildren schema)



-- READ SCHEMA


readSchemaChildren : Schema -> List (Html Msg)
readSchemaChildren { name } =
    [ schemaName name
    , editSchemaButton
    , deleteSchemaButton
    ]


schemaName : String -> Html Msg
schemaName name =
    h2 [] [ text name ]


saveSchemaButton : Html Msg
saveSchemaButton =
    button [ onClick SaveSchema ] [ text "Save" ]


cancelEditSchemaButton : Html Msg
cancelEditSchemaButton =
    button [ onClick CancelEditSchema ] [ text "Cancel" ]


editSchemaButton : Html Msg
editSchemaButton =
    button [ onClick EditSchema ] [ text "Edit Name" ]


deleteSchemaButton : Html Msg
deleteSchemaButton =
    button [ onClick Destroy ] [ text "Delete Schema" ]



-- EDIT SCHEMA


editingSchemaChildren : Schema -> List (Html Msg)
editingSchemaChildren { name } =
    [ editSchemaNameInput name
    , saveSchemaButton
    , cancelEditSchemaButton
    ]


editSchemaNameInput : String -> Html Msg
editSchemaNameInput name =
    input
        [ id "edit-schema-name"
        , value name
        , onInput InputSchemaName
        , customOnKeyDown onSchemaNameKeyDown
        ]
        []


onSchemaNameKeyDown : Key -> Maybe Msg
onSchemaNameKeyDown key =
    case key of
        Enter ->
            Just SaveSchema

        Escape ->
            Just CancelEditSchema

        _ ->
            Nothing



-- ENTITIES VIEW


entitiesView : Model -> Html Msg
entitiesView model =
    section [] (entitiesChildren model)


entitiesChildren : Model -> List (Html Msg)
entitiesChildren { editingEntity, entities, newEntity, errors } =
    CE.prependIfErrors
        errors
        [ entitiesTitle
        , createEntityView newEntity.name
        , entitiesListView editingEntity entities
        ]


entitiesTitle : Html msg
entitiesTitle =
    h3 [] [ text "Models" ]


createEntityView : String -> Html Msg
createEntityView name =
    div []
        [ input
            [ id "create-entity"
            , value name
            , onInput InputNewEntityName
            , onEnter CreateEntity
            ]
            []
        , button
            [ onClick CreateEntity
            ]
            [ text "Create Model" ]
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
        [ id "edit-entity-name"
        , value name
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
