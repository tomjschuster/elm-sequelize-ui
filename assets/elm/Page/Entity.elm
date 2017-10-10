module Page.Entity exposing (Model, Msg, init, initialModel, update, view)

import AppUpdate exposing (AppUpdate)
import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Data.Combined as Combined exposing (EntityWithAll)
import Data.DataType as DataType exposing (DataType)
import Data.Entity as Entity exposing (Entity)
import Data.Field as Field exposing (Field)
import Data.Schema as Schema exposing (Schema)
import Dom
import Html
    exposing
        ( Html
        , button
        , div
        , h2
        , h3
        , input
        , li
        , main_
        , option
        , section
        , select
        , span
        , text
        , ul
        )
import Html.Attributes exposing (id, selected, value)
import Html.Events exposing (onClick, onInput)
import Http
import Request.Entity as RE
import Request.Field as RF
import Router exposing (Route)
import Task
import Utils.Handlers exposing (customOnKeyDown, onChangeInt, onEnter)
import Utils.Keys exposing (Key(..))
import Views.Breadcrumbs as BC
import Views.ChangesetError as CE
import Views.DataType.Select as DTSelect


-- MODEL


type alias Model =
    { schema : Schema
    , entity : Entity
    , fields : List Field
    , editingName : Maybe String
    , newField : Field
    , editingField : Maybe Field
    , toDeleteId : Maybe Int
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    Model
        Schema.empty
        Entity.empty
        []
        Nothing
        Field.empty
        Nothing
        Nothing
        []


type alias InitialData =
    { schema : Schema
    , entity : Entity
    }


init : Int -> Int -> ( Model, Cmd Msg )
init schemaId entityId =
    ( { initialModel | newField = Field.init entityId }
    , RE.oneWithAll entityId |> Http.toTask |> Task.attempt LoadEntityWithAll
    )



-- UPDATE


type Msg
    = NoOp
    | FocusResult (Result Dom.Error ())
    | Goto Route
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
      -- CREATE FIELD
    | InputNewFieldName String
    | SelectNewFieldDataType DataType
    | UpdateNewFieldModifier DataType.Modifier
    | CreateField
    | LoadNewField (Result Http.Error Field)
      -- UPDATE FIELD
    | EditField Int
    | InputEditingFieldName String
    | SelectEditingFieldDataType DataType
    | UpdateEditingFieldModifier DataType.Modifier
    | CancelEditField
    | SaveEditingField
    | UpdateField (Result Http.Error Field)
      -- DELETE FIELD
    | DestroyField Int
    | RemoveField (Result Http.Error ())


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

        LoadEntityWithAll (Ok { schema, entity, fields }) ->
            ( { model
                | entity = entity
                , schema = schema
                , fields = fields
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadEntityWithAll (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        -- ENTITY
        LoadEntity (Ok entity) ->
            ( { model
                | entity = entity
                , editingName = Nothing
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

        EditEntityName ->
            ( { model
                | editingName = Just model.entity.name
                , editingField = Nothing
                , errors = []
              }
            , Dom.focus "edit-entity-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        InputEntityName name ->
            ( { model | editingName = Just name }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditEntityName ->
            ( { model | editingName = Nothing, errors = [] }
            , Cmd.none
            , AppUpdate.none
            )

        SaveEntityName ->
            ( model
            , model.editingName
                |> Maybe.map
                    (flip Entity.updateName model.entity
                        >> RE.update
                        >> Http.send LoadEntity
                    )
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        Destroy ->
            ( model
            , RE.destroy model.entity.id |> Http.send RemoveEntity
            , AppUpdate.none
            )

        RemoveEntity (Ok ()) ->
            ( { model | errors = [] }
            , Router.goto (Router.Schema model.schema.id)
            , AppUpdate.none
            )

        RemoveEntity (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        -- FIELDS
        -- NEW FIELD
        InputNewFieldName name ->
            ( { model | newField = Field.updateName name model.newField }
            , Cmd.none
            , AppUpdate.none
            )

        SelectNewFieldDataType dataType ->
            ( { model | newField = Field.updateDataType dataType model.newField }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateNewFieldModifier modifier ->
            ( { model
                | newField = Field.updateDataTypeModifier modifier model.newField
              }
            , Cmd.none
            , AppUpdate.none
            )

        CreateField ->
            ( model
            , RF.create model.newField |> Http.send LoadNewField
            , AppUpdate.none
            )

        LoadNewField (Ok field) ->
            ( { model
                | fields = model.fields ++ [ field ]
                , newField = Field.init model.entity.id
                , errors = []
              }
            , Dom.focus "create-field" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        LoadNewField (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Dom.focus "create-field" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        -- EDIT FIELD
        EditField id ->
            ( { model
                | editingName = Nothing
                , editingField =
                    model.fields
                        |> List.filter (.id >> (==) id)
                        |> List.head
                , errors = []
              }
            , Dom.focus "edit-field-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        InputEditingFieldName name ->
            ( { model
                | editingField =
                    Maybe.map (Field.updateName name) model.editingField
              }
            , Cmd.none
            , AppUpdate.none
            )

        SelectEditingFieldDataType dataType ->
            ( { model
                | editingField =
                    Maybe.map (Field.updateDataType dataType) model.editingField
              }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateEditingFieldModifier modifier ->
            ( { model
                | editingField =
                    Maybe.map (Field.updateDataTypeModifier modifier) model.editingField
              }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditField ->
            ( { model | editingField = Nothing, errors = [] }
            , Cmd.none
            , AppUpdate.none
            )

        SaveEditingField ->
            ( model
            , model.editingField
                |> Maybe.map (RF.update >> Http.send UpdateField)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        UpdateField (Ok field) ->
            ( { model
                | fields = List.map (Field.replaceIfMatch field) model.fields
                , editingField = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateField (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Dom.focus "edit-field-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        DestroyField id ->
            ( { model | toDeleteId = Just id }
            , RF.destroy id |> Http.send RemoveField
            , AppUpdate.none
            )

        RemoveField (Ok ()) ->
            ( { model
                | errors = []
                , fields =
                    model.toDeleteId
                        |> Maybe.map (Field.removeFromList model.fields)
                        |> Maybe.withDefault model.fields
              }
            , Cmd.none
            , AppUpdate.none
            )

        RemoveField (Err error) ->
            ( { model
                | errors = ChangesetError.parseHttpError error
                , toDeleteId = Nothing
              }
            , Cmd.none
            , AppUpdate.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ breadCrumbs model.schema model.entity
        , title model.editingName model.entity.name
        , content model
        ]


breadCrumbs : Schema -> Entity -> Html Msg
breadCrumbs schema entity =
    BC.view Goto [ BC.home, BC.schema schema, BC.entity entity ]



-- ENTITY NAME VIEW


title : Maybe String -> String -> Html Msg
title editingName name =
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
    input
        [ id "edit-entity-name"
        , value name
        , onInput InputEntityName
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


saveEditEntityNameButton : Html Msg
saveEditEntityNameButton =
    button [ onClick SaveEntityName ] [ text "Save" ]



-- FIELDS VIEW


content : Model -> Html Msg
content model =
    section [] (contentChildren model)


contentChildren : Model -> List (Html Msg)
contentChildren model =
    CE.prependIfErrors model.errors
        [ fieldsTitle
        , createField model.newField
        , fieldList model.editingField model.schema.id model.fields
        ]


fieldsTitle : Html msg
fieldsTitle =
    h3 [] [ text "Fields" ]



-- CREATE FIELD


createField : Field -> Html Msg
createField { name, dataType, dataTypeModifier } =
    div
        []
        [ createFieldInput name
        , DTSelect.view selectDataTypeConfig dataType dataTypeModifier
        , createFieldButton
        ]


selectDataTypeConfig : DTSelect.Config Msg
selectDataTypeConfig =
    { handleDataTypeChange = SelectNewFieldDataType
    , handleModifierChange = UpdateNewFieldModifier
    }


createFieldInput : String -> Html Msg
createFieldInput name =
    input
        [ id "create-field"
        , value name
        , onInput InputNewFieldName
        , onEnter CreateField
        ]
        []


createFieldButton : Html Msg
createFieldButton =
    button [ onClick CreateField ] [ text "Create" ]



-- FIELD LIST


fieldList : Maybe Field -> Int -> List Field -> Html Msg
fieldList editingField schemaId fields =
    ul [] (List.map (fieldItem editingField schemaId) fields)



-- READ FIELDS


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
    [ fieldLink schemaId field
    , dataType field.dataType field.dataTypeModifier
    , editFieldButton field.id
    , deleteFieldButton field.id
    ]


fieldLink : Int -> Field -> Html Msg
fieldLink schemaId { entityId, id, name } =
    Router.link Goto (Router.Field schemaId entityId id) [] [ text name ]


dataType : DataType -> DataType.Modifier -> Html msg
dataType dataType modifier =
    span []
        [ text
            (" ("
                ++ DataType.toStringValue dataType
                ++ (DataType.modifierToString modifier
                        |> Maybe.map ((++) " ")
                        |> Maybe.withDefault ""
                   )
                ++ ")"
            )
        ]



-- UPDATE FIELDS


getEditingFieldItemChildren : Field -> Field -> List (Html Msg)
getEditingFieldItemChildren field editingField =
    if field.id == editingField.id then
        editingFieldItemChildren editingField
    else
        [ text field.name ]


editingFieldItemChildren : Field -> List (Html Msg)
editingFieldItemChildren field =
    [ editFieldNameInput field.name
    , DTSelect.view editFieldSelectDataTypeConfig field.dataType field.dataTypeModifier
    , cancelEditFieldButton
    , saveEditFieldButton
    ]


editFieldButton : Int -> Html Msg
editFieldButton id =
    button [ onClick (EditField id) ] [ text "Edit" ]


editFieldNameInput : String -> Html Msg
editFieldNameInput name =
    input
        [ id "edit-field-name"
        , value name
        , onInput InputEditingFieldName
        , customOnKeyDown onFieldNameKeyDown
        ]
        []


onFieldNameKeyDown : Key -> Maybe Msg
onFieldNameKeyDown key =
    case key of
        Enter ->
            Just SaveEditingField

        Escape ->
            Just CancelEditField

        _ ->
            Nothing


editFieldSelectDataTypeConfig : DTSelect.Config Msg
editFieldSelectDataTypeConfig =
    { handleDataTypeChange = SelectEditingFieldDataType
    , handleModifierChange = UpdateEditingFieldModifier
    }


cancelEditFieldButton : Html Msg
cancelEditFieldButton =
    button [ onClick CancelEditField ] [ text "Cancel" ]


saveEditFieldButton : Html Msg
saveEditFieldButton =
    button [ onClick SaveEditingField ] [ text "Save" ]


deleteFieldButton : Int -> Html Msg
deleteFieldButton id =
    button [ onClick (DestroyField id) ] [ text "Delete" ]
