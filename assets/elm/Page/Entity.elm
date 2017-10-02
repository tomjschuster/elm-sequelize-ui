module Page.Entity exposing (Model, Msg, init, initialModel, update, view)

import Data.Combined as Combined exposing (EntityWithAll)
import Data.Entity as Entity exposing (Entity)
import Data.Field as Field exposing (Field)
import Data.Schema as Schema exposing (Schema)
import Html exposing (Html, button, h2, h3, input, li, main_, section, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Http
import Request.Entity as RE
import Request.Field as RF
import Request.Schema as RS
import Router exposing (Route)
import Task
import Views.Breadcrumbs as BC


-- MODEL


type alias Model =
    { schema : Schema
    , entity : Entity
    , fields : List Field
    , editingName : Maybe String
    , newFieldInput : String
    , editingField : Maybe Field
    , toDeleteId : Maybe Int
    , error : Maybe String
    }


initialModel : Model
initialModel =
    Model Schema.empty Entity.empty [] Nothing "" Nothing Nothing Nothing


type alias InitialData =
    { schema : Schema
    , entity : Entity
    }


init : Int -> Int -> Cmd Msg
init schemaId id =
    RE.oneWithAll id
        |> Http.toTask
        |> Task.attempt LoadEntityWithAll



-- UPDATE


type Msg
    = Goto Route
    | LoadEntityWithAll (Result Http.Error EntityWithAll)
      -- ENTITY
    | LoadEntity (Result Http.Error Entity)
    | EditEntityName
    | InputEntityName String
    | CancelEditEntityName
    | SaveEntityName
    | Destroy
    | RemoveEntity (Result Http.Error ())
      -- FIELDS
    | InputNewFieldName String
    | CreateField
    | LoadNewField (Result Http.Error Field)
    | EditFieldName Int
    | InputEditingFieldName String
    | CancelEditFieldName
    | SaveFieldName
    | UpdateField (Result Http.Error Field)
    | DestroyField Int
    | RemoveField (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Goto route ->
            ( model, Router.goto route )

        LoadEntityWithAll (Ok { schema, entity, fields }) ->
            ( { model
                | entity = entity
                , schema = schema
                , fields = fields
                , error = Nothing
              }
            , Cmd.none
            )

        LoadEntityWithAll (Err error) ->
            ( { model | error = Just "Error loading model" }, Cmd.none )

        -- ENTITY
        LoadEntity (Ok entity) ->
            ( { model
                | entity = entity
                , editingName = Nothing
                , error = Nothing
              }
            , Cmd.none
            )

        LoadEntity (Err error) ->
            ( { model | error = Just "Error loading model" }, Cmd.none )

        EditEntityName ->
            ( { model | editingName = Just model.entity.name }, Cmd.none )

        InputEntityName name ->
            ( { model | editingName = Just name }, Cmd.none )

        CancelEditEntityName ->
            ( { model | editingName = Nothing }, Cmd.none )

        SaveEntityName ->
            ( model
            , model.editingName
                |> Maybe.map
                    (updateEntityName model.entity
                        >> RE.update
                        >> Http.send LoadEntity
                    )
                |> Maybe.withDefault Cmd.none
            )

        Destroy ->
            ( model, RE.destroy model.entity.id |> Http.send RemoveEntity )

        RemoveEntity (Ok ()) ->
            ( model, Router.goto (Router.Schema model.schema.id) )

        RemoveEntity (Err error) ->
            ( { model | error = Just "Error deleting model" }, Cmd.none )

        -- FIELDS
        InputNewFieldName name ->
            ( { model | newFieldInput = name }, Cmd.none )

        CreateField ->
            ( model
            , RF.create model.newFieldInput model.entity.id
                |> Http.send LoadNewField
            )

        LoadNewField (Ok field) ->
            ( { model
                | fields = model.fields ++ [ field ]
                , newFieldInput = ""
                , error = Nothing
              }
            , Cmd.none
            )

        LoadNewField (Err error) ->
            ( { model | error = Just "Error creating field" }, Cmd.none )

        EditFieldName id ->
            ( { model
                | editingField =
                    model.fields
                        |> List.filter (.id >> (==) id)
                        |> List.head
              }
            , Cmd.none
            )

        InputEditingFieldName name ->
            ( { model
                | editingField =
                    Maybe.map (updateFieldName name) model.editingField
              }
            , Cmd.none
            )

        CancelEditFieldName ->
            ( { model | editingField = Nothing }, Cmd.none )

        SaveFieldName ->
            ( model
            , model.editingField
                |> Maybe.map (RF.update >> Http.send UpdateField)
                |> Maybe.withDefault Cmd.none
            )

        UpdateField (Ok field) ->
            ( { model
                | fields = List.map (replaceField field) model.fields
                , editingField = Nothing
                , error = Nothing
              }
            , Cmd.none
            )

        UpdateField (Err error) ->
            ( { model | error = Just "Error updating field" }, Cmd.none )

        DestroyField id ->
            ( { model | toDeleteId = Just id }
            , RF.destroy id |> Http.send RemoveField
            )

        RemoveField (Ok ()) ->
            ( { model
                | fields =
                    model.toDeleteId
                        |> Maybe.map (removeField model.fields)
                        |> Maybe.withDefault model.fields
              }
            , Cmd.none
            )

        RemoveField (Err error) ->
            ( { model | error = Just "Error deleting field", toDeleteId = Nothing }, Cmd.none )


updateEntityName : Entity -> String -> Entity
updateEntityName entity name =
    { entity | name = name }


updateFieldName : String -> Field -> Field
updateFieldName name field =
    { field | name = name }


replaceField : Field -> Field -> Field
replaceField newField field =
    if field.id == newField.id then
        newField
    else
        field


removeField : List Field -> Int -> List Field
removeField fields id =
    List.filter (.id >> (/=) id) fields



-- VIEW


view : Model -> Html Msg
view { schema, entity, fields, editingName, newFieldInput, editingField } =
    main_ []
        [ breadCrumbs schema entity
        , nameView editingName entity.name
        , fieldsView newFieldInput editingField schema.id fields
        ]


breadCrumbs : Schema -> Entity -> Html Msg
breadCrumbs schema entity =
    BC.view Goto [ BC.home, BC.schema schema, BC.entity entity ]



-- ENTITY NAME VIEW


nameView : Maybe String -> String -> Html Msg
nameView editingName name =
    section [] (nameChildren editingName name)


nameChildren : Maybe String -> String -> List (Html Msg)
nameChildren editingName name =
    editingName
        |> Maybe.map editingNameChildren
        |> Maybe.withDefault (normalNameChildren name)


editingNameChildren : String -> List (Html Msg)
editingNameChildren name =
    [ editEntityNameInput name
    , cancelEditEntityNameButton
    , saveEditEntityNameButton
    ]


normalNameChildren : String -> List (Html Msg)
normalNameChildren name =
    [ entityName name
    , editEntityNameButton
    , deleteEntityButton
    ]


entityName : String -> Html Msg
entityName name =
    h2 [] [ text name ]


editEntityNameButton : Html Msg
editEntityNameButton =
    button [ onClick EditEntityName ] [ text "Edit Name" ]


deleteEntityButton : Html Msg
deleteEntityButton =
    button [ onClick Destroy ] [ text "Delete Model" ]


editEntityNameInput : String -> Html Msg
editEntityNameInput name =
    input [ value name, onInput InputEntityName ] []


cancelEditEntityNameButton : Html Msg
cancelEditEntityNameButton =
    button [ onClick CancelEditEntityName ] [ text "Cancel" ]


saveEditEntityNameButton : Html Msg
saveEditEntityNameButton =
    button [ onClick SaveEntityName ] [ text "Save" ]



-- FIELDS VIEW


fieldsView : String -> Maybe Field -> Int -> List Field -> Html Msg
fieldsView newFieldInput editingField schemaId fields =
    section
        []
        [ h3 [] [ text "Fields" ]
        , createFieldInput newFieldInput
        , createFieldButton
        , fieldList editingField schemaId fields
        ]


createFieldInput : String -> Html Msg
createFieldInput name =
    input [ value name, onInput InputNewFieldName ] []


createFieldButton : Html Msg
createFieldButton =
    button [ onClick CreateField ] [ text "Create" ]


fieldList : Maybe Field -> Int -> List Field -> Html Msg
fieldList editingField schemaId fields =
    ul [] (List.map (fieldItem editingField schemaId) fields)


fieldItem : Maybe Field -> Int -> Field -> Html Msg
fieldItem editingField schemaId field =
    li [] (fieldItemChildren editingField schemaId field)


fieldItemChildren : Maybe Field -> Int -> Field -> List (Html Msg)
fieldItemChildren editingField schemaId field =
    editingField
        |> Maybe.map (getEditingFieldItemChildren field)
        |> Maybe.withDefault (normalFieldItemChildren schemaId field)


normalFieldItemChildren : Int -> Field -> List (Html Msg)
normalFieldItemChildren schemaId field =
    [ fieldLink schemaId field, editFieldButton field.id, deleteFieldButton field.id ]


getEditingFieldItemChildren : Field -> Field -> List (Html Msg)
getEditingFieldItemChildren field editingField =
    if field.id == editingField.id then
        editingFieldItemChildren editingField
    else
        [ text field.name ]


editingFieldItemChildren : Field -> List (Html Msg)
editingFieldItemChildren field =
    [ editFieldNameInput field.name
    , cancelEditFieldNameButton
    , saveEditFieldNameButton
    ]


fieldLink : Int -> Field -> Html Msg
fieldLink schemaId { entityId, id, name } =
    Router.link Goto (Router.Field schemaId entityId id) [] [ text name ]


editFieldButton : Int -> Html Msg
editFieldButton id =
    button [ onClick (EditFieldName id) ] [ text "Edit" ]


editFieldNameInput : String -> Html Msg
editFieldNameInput name =
    input [ value name, onInput InputEditingFieldName ] []


cancelEditFieldNameButton : Html Msg
cancelEditFieldNameButton =
    button [ onClick CancelEditFieldName ] [ text "Cancel" ]


saveEditFieldNameButton : Html Msg
saveEditFieldNameButton =
    button [ onClick SaveFieldName ] [ text "Save" ]


deleteFieldButton : Int -> Html Msg
deleteFieldButton id =
    button [ onClick (DestroyField id) ] [ text "Delete" ]
