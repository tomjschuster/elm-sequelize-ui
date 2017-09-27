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

import Data.Entity exposing (Entity)
import Data.Schema exposing (Schema, emptySchema)
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
import Router


-- MODEL


type alias Model =
    { schema : Schema
    , editingName : Maybe String
    , newEntityInput : String
    , editingEntity : Maybe Entity
    , toDeleteId : Maybe Int
    , error : Maybe String
    }


initialModel : Model
initialModel =
    Model emptySchema Nothing "" Nothing Nothing Nothing


init : Int -> Cmd Msg
init id =
    RS.one id |> Http.send LoadSchema



-- UPDATE


type Msg
    = -- SCHEMA
      LoadSchema (Result Http.Error Schema)
    | RemoveSchema (Result Http.Error ())
    | EditSchemaName
    | InputSchemaName String
    | CancelEditSchemaName
    | SaveSchemaName
    | Destroy
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- SCHEMA
        LoadSchema (Ok schema) ->
            ( { model
                | schema = schema
                , editingName = Nothing
                , error = Nothing
              }
            , Cmd.none
            )

        LoadSchema (Err error) ->
            ( { model | error = Just "Error loading schema" }, Cmd.none )

        Destroy ->
            ( model, RS.destroy model.schema.id |> Http.send RemoveSchema )

        EditSchemaName ->
            ( { model | editingName = Just model.schema.name }, Cmd.none )

        InputSchemaName name ->
            ( { model | editingName = Just name }, Cmd.none )

        CancelEditSchemaName ->
            ( { model | editingName = Nothing }, Cmd.none )

        SaveSchemaName ->
            ( model
            , model.editingName
                |> saveSchemaName model.schema
                |> RS.update
                |> Http.send LoadSchema
            )

        RemoveSchema (Ok ()) ->
            ( model, Router.goto Router.Home )

        RemoveSchema (Err error) ->
            ( { model | error = Just "Error deleting schema" }, Cmd.none )

        -- ENTITIES
        InputNewEntityName newEntityInput ->
            ( { model | newEntityInput = newEntityInput }, Cmd.none )

        CreateEntity ->
            ( model
            , RE.create model.newEntityInput model.schema.id
                |> Http.send LoadEntity
            )

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

        EditEntityName id ->
            ( { model
                | editingEntity = getEditingEntity id model.schema.entities
              }
            , Cmd.none
            )

        InputEditingEntityName name ->
            ( { model
                | editingEntity =
                    Maybe.map (updateEditingEntityName name) model.editingEntity
              }
            , Cmd.none
            )

        CancelEditEntityName ->
            ( { model | editingEntity = Nothing }, Cmd.none )

        SaveEntityName ->
            ( model
            , model.editingEntity
                |> Maybe.map (RE.update >> Http.send UpdateEntity)
                |> Maybe.withDefault Cmd.none
            )

        UpdateEntity (Ok entity) ->
            ( { model
                | schema = updateEntity model.schema entity
                , editingEntity = Nothing
                , error = Nothing
              }
            , Cmd.none
            )

        UpdateEntity (Err error) ->
            ( { model | error = Just "Error updating entity" }, Cmd.none )

        DestroyEntity id ->
            ( { model | toDeleteId = Just id }
            , RE.destroy id
                |> Http.send RemoveEntity
            )

        RemoveEntity (Ok ()) ->
            ( { model
                | schema = removeEntity model.schema model.toDeleteId
                , error = Nothing
              }
            , Cmd.none
            )

        RemoveEntity (Err error) ->
            ( { model | error = Just "Error deleting model" }, Cmd.none )



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


addEntity : Schema -> Entity -> Schema
addEntity schema entity =
    { schema | entities = schema.entities ++ [ entity ] }


getEditingEntity : Int -> List Entity -> Maybe Entity
getEditingEntity id =
    List.filter (.id >> (==) id) >> List.head


updateEditingEntityName : String -> Entity -> Entity
updateEditingEntityName name entity =
    { entity | name = name }


updateEntity : Schema -> Entity -> Schema
updateEntity schema entity =
    { schema | entities = List.map (replaceEntity entity) schema.entities }


replaceEntity : Entity -> Entity -> Entity
replaceEntity newEntity entity =
    if entity.id == newEntity.id then
        Debug.log ";lkj" newEntity
    else
        entity


removeEntity : Schema -> Maybe Int -> Schema
removeEntity schema maybeId =
    { schema
        | entities =
            List.filter (.id >> Just >> (/=) maybeId) schema.entities
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view { schema, editingName, newEntityInput, editingEntity } =
    main_ []
        [ nameView editingName schema.name
        , entitiesView editingEntity schema.entities newEntityInput
        ]



-- NAME VIEW


nameView : Maybe String -> String -> Html Msg
nameView editingName name =
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
    input [ value name, onInput InputSchemaName ] []


cancelEditSchemaNameButton : Html Msg
cancelEditSchemaNameButton =
    button [ onClick CancelEditSchemaName ] [ text "Cancel" ]


saveSchemaNameButton : Html Msg
saveSchemaNameButton =
    button [ onClick SaveSchemaName ] [ text "Save" ]



-- ENTITIES VIEW


entitiesView : Maybe Entity -> List Entity -> String -> Html Msg
entitiesView editingEntity entities newEntityInput =
    section []
        [ h3 [] [ text "Models" ]
        , createEntityView newEntityInput
        , entitiesListView editingEntity entities
        ]


createEntityView : String -> Html Msg
createEntityView newEntityInput =
    div []
        [ input [ value newEntityInput, onInput InputNewEntityName ] []
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
    Maybe.map (getEntityChildren entity)
        >> Maybe.withDefault (normalEntityChildren entity)


getEntityChildren : Entity -> Entity -> List (Html Msg)
getEntityChildren entity editingEntity =
    if editingEntity.id == entity.id then
        editingEntityChildren editingEntity
    else
        normalEntityChildren entity


normalEntityChildren : Entity -> List (Html Msg)
normalEntityChildren { id, name } =
    [ text name
    , editEntityNameButton id
    , deleteEntityButton id
    ]


editingEntityChildren : Entity -> List (Html Msg)
editingEntityChildren { name } =
    [ editEntityNameInput name
    , cancelEditEntityNameButton
    , saveEntityNameButton
    ]


editEntityNameButton : Int -> Html Msg
editEntityNameButton id =
    button [ onClick (EditEntityName id) ] [ text "Edit Name" ]


editEntityNameInput : String -> Html Msg
editEntityNameInput name =
    input [ value name, onInput InputEditingEntityName ] []


cancelEditEntityNameButton : Html Msg
cancelEditEntityNameButton =
    button [ onClick CancelEditEntityName ] [ text "Cancel" ]


saveEntityNameButton : Html Msg
saveEntityNameButton =
    button [ onClick SaveEntityName ] [ text "Save" ]


deleteEntityButton : Int -> Html Msg
deleteEntityButton id =
    button [ onClick (DestroyEntity id) ] [ text "Delete" ]
