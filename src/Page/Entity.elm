module Page.Entity exposing (Model, Msg, init, initialModel, update, view)

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


-- MODEL


type alias Model =
    { entity : Entity
    , schema : Schema
    , newFieldInput : String
    , editingField : Maybe Field
    , toDeleteId : Maybe Int
    , error : Maybe String
    }


initialModel : Model
initialModel =
    Model Entity.empty Schema.empty "" Nothing Nothing Nothing


type alias InitialData =
    { schema : Schema
    , entity : Entity
    }


init : Int -> Int -> Cmd Msg
init schemaId id =
    Task.map2
        InitialData
        (RS.one schemaId |> Http.toTask)
        (RE.oneWithFields id |> Http.toTask)
        |> Task.attempt LoadEntityAndSchema



-- UPDATE


type Msg
    = Goto Route
    | LoadEntityAndSchema (Result Http.Error InitialData)
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

        LoadEntityAndSchema (Ok { schema, entity }) ->
            ( { model
                | entity = entity
                , schema = schema
                , error = Nothing
              }
            , Cmd.none
            )

        LoadEntityAndSchema (Err error) ->
            ( { model | error = Just "Error loading model" }, Cmd.none )

        InputNewFieldName name ->
            ( { model | newFieldInput = name }, Cmd.none )

        CreateField ->
            ( model
            , RF.create model.newFieldInput model.schema.id
                |> Http.send LoadNewField
            )

        LoadNewField (Ok field) ->
            ( { model
                | entity = addNewField model.entity field
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
                    model.entity.fields
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
                | entity = updateField model.entity field
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
                | entity =
                    model.toDeleteId
                        |> Maybe.map (removeField model.entity)
                        |> Maybe.withDefault model.entity
              }
            , Cmd.none
            )

        RemoveField (Err error) ->
            ( { model | error = Just "Error deleting field", toDeleteId = Nothing }, Cmd.none )


addNewField : Entity -> Field -> Entity
addNewField entity field =
    { entity | fields = entity.fields ++ [ field ] }


updateFieldName : String -> Field -> Field
updateFieldName name field =
    { field | name = name }


updateField : Entity -> Field -> Entity
updateField entity field =
    { entity | fields = List.map (replaceField field) entity.fields }


replaceField : Field -> Field -> Field
replaceField newField field =
    if field.id == newField.id then
        newField
    else
        field


removeField : Entity -> Int -> Entity
removeField entity id =
    { entity | fields = List.filter (.id >> (/=) id) entity.fields }



-- VIEW


view : Model -> Html Msg
view { schema, entity, newFieldInput, editingField } =
    main_ []
        [ schemaLink schema
        , title entity.name
        , fieldsView newFieldInput editingField schema.id entity.fields
        ]


title : String -> Html Msg
title name =
    h2 [] [ text name ]


schemaLink : Schema -> Html Msg
schemaLink { id, name } =
    Router.link Goto (Router.Schema id) [] [ text name ]


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
