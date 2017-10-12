module Page.Table exposing (Model, Msg, init, initialModel, update, view)

import AppUpdate exposing (AppUpdate)
import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Data.Combined as Combined exposing (TableWithAll)
import Data.DataType as DataType exposing (DataType)
import Data.Field as Field exposing (Field)
import Data.Schema as Schema exposing (Schema)
import Data.Table as Table exposing (Table)
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
import Request.Field as RF
import Request.Table as RE
import Router exposing (Route)
import Task
import Utils.Handlers exposing (customOnKeyDown, onChangeInt, onEnter)
import Utils.Keys exposing (Key(..))
import Views.Breadcrumbs as BC
import Views.ChangesetError as CE
import Views.DataType.Display as DTDisplay
import Views.DataType.Select as DTSelect


-- MODEL


type alias Model =
    { schema : Schema
    , table : Table
    , fields : List Field
    , editingTable : Maybe Table
    , newField : Field
    , editingField : Maybe Field
    , toDeleteId : Maybe Int
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    Model Schema.empty
        Table.empty
        []
        Nothing
        Field.empty
        Nothing
        Nothing
        []


type alias InitialData =
    { schema : Schema
    , table : Table
    }


init : Int -> Int -> ( Model, Cmd Msg )
init schemaId tableId =
    ( { initialModel | newField = Field.init tableId }
    , RE.oneWithAll tableId |> Http.toTask |> Task.attempt LoadTableWithAll
    )



-- UPDATE


type Msg
    = NoOp
    | FocusResult (Result Dom.Error ())
    | Goto Route
    | LoadTableWithAll (Result Http.Error TableWithAll)
      -- ENTITY
    | LoadTable (Result Http.Error Table)
    | EditTable
    | InputTableName String
    | CancelEditTable
    | SaveTableName
    | Destroy
    | RemoveTable (Result Http.Error ())
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

        LoadTableWithAll (Ok { schema, table, fields }) ->
            ( { model
                | table = table
                , schema = schema
                , fields = fields
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadTableWithAll (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        -- ENTITY
        LoadTable (Ok table) ->
            ( { model | table = table, editingTable = Nothing, errors = [] }
            , Cmd.none
            , AppUpdate.none
            )

        LoadTable (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        EditTable ->
            ( { model
                | editingTable = Just model.table
                , editingField = Nothing
                , errors = []
              }
            , Dom.focus "edit-table-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        InputTableName name ->
            ( { model
                | editingTable =
                    Maybe.map (Table.updateName name) model.editingTable
              }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditTable ->
            ( { model | editingTable = Nothing }
            , Cmd.none
            , AppUpdate.none
            )

        SaveTableName ->
            ( model
            , model.editingTable
                |> Maybe.map (RE.update >> Http.send LoadTable)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        Destroy ->
            ( model
            , RE.destroy model.table.id |> Http.send RemoveTable
            , AppUpdate.none
            )

        RemoveTable (Ok ()) ->
            ( { model | errors = [] }
            , Router.goto (Router.Schema model.schema.id)
            , AppUpdate.none
            )

        RemoveTable (Err error) ->
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
            ( { model
                | newField =
                    Field.updateDataType dataType model.newField
              }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateNewFieldModifier modifier ->
            ( { model
                | newField =
                    Field.updateDataTypeModifier modifier model.newField
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
                , newField = Field.init model.table.id
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
                | editingTable = Nothing
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
                    Maybe.map
                        (Field.updateDataTypeModifier modifier)
                        model.editingField
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
        [ breadCrumbs model.schema model.table
        , tableView model.editingTable model.table
        , fieldsView model
        ]


breadCrumbs : Schema -> Table -> Html Msg
breadCrumbs schema table =
    BC.view Goto [ BC.home, BC.schema schema, BC.table table ]



-- ENTITY VIEW


tableView : Maybe Table -> Table -> Html Msg
tableView editingTable table =
    section [] (tableChildren editingTable table)


tableChildren : Maybe Table -> Table -> List (Html Msg)
tableChildren editingTable table =
    editingTable
        |> Maybe.map editingTableChildren
        |> Maybe.withDefault (readTableChildren table)



-- READ ENTITY


readTableChildren : Table -> List (Html Msg)
readTableChildren { name } =
    [ tableName name
    , editTableNameButton
    , deleteTableButton
    ]


tableName : String -> Html Msg
tableName name =
    h2 [] [ text name ]


editTableNameButton : Html Msg
editTableNameButton =
    button [ onClick EditTable ] [ text "Edit Name" ]


deleteTableButton : Html Msg
deleteTableButton =
    button [ onClick Destroy ] [ text "Delete Table" ]



-- UPDATE ENTITY


editingTableChildren : Table -> List (Html Msg)
editingTableChildren { name } =
    [ editTableNameInput name
    , cancelEditTableButton
    , saveEditTableButton
    ]


editTableNameInput : String -> Html Msg
editTableNameInput name =
    input
        [ id "edit-table-name"
        , value name
        , onInput InputTableName
        , customOnKeyDown onTableNameKeyDown
        ]
        []


onTableNameKeyDown : Key -> Maybe Msg
onTableNameKeyDown key =
    case key of
        Enter ->
            Just SaveTableName

        Escape ->
            Just CancelEditTable

        _ ->
            Nothing


cancelEditTableButton : Html Msg
cancelEditTableButton =
    button [ onClick CancelEditTable ] [ text "Cancel" ]


saveEditTableButton : Html Msg
saveEditTableButton =
    button [ onClick SaveTableName ] [ text "Save" ]



-- FIELDS VIEW


fieldsView : Model -> Html Msg
fieldsView model =
    section [] (fieldsChildren model)


fieldsChildren : Model -> List (Html Msg)
fieldsChildren model =
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
    , DTDisplay.view field.dataType field.dataTypeModifier
    , editFieldButton field.id
    , deleteFieldButton field.id
    ]


fieldLink : Int -> Field -> Html Msg
fieldLink schemaId { tableId, id, name } =
    Router.link Goto (Router.Field schemaId tableId id) [] [ text name ]


editFieldButton : Int -> Html Msg
editFieldButton id =
    button [ onClick (EditField id) ] [ text "Edit" ]


deleteFieldButton : Int -> Html Msg
deleteFieldButton id =
    button [ onClick (DestroyField id) ] [ text "Delete" ]



-- UPDATE FIELDS


getEditingFieldItemChildren : Field -> Field -> List (Html Msg)
getEditingFieldItemChildren field editingField =
    if field.id == editingField.id then
        editingFieldItemChildren editingField
    else
        [ text field.name ]


editingFieldItemChildren : Field -> List (Html Msg)
editingFieldItemChildren { name, dataType, dataTypeModifier } =
    [ editFieldNameInput name
    , DTSelect.view fieldSelectDataTypeConfig dataType dataTypeModifier
    , cancelEditFieldButton
    , saveEditFieldButton
    ]


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


fieldSelectDataTypeConfig : DTSelect.Config Msg
fieldSelectDataTypeConfig =
    { handleDataTypeChange = SelectEditingFieldDataType
    , handleModifierChange = UpdateEditingFieldModifier
    }


cancelEditFieldButton : Html Msg
cancelEditFieldButton =
    button [ onClick CancelEditField ] [ text "Cancel" ]


saveEditFieldButton : Html Msg
saveEditFieldButton =
    button [ onClick SaveEditingField ] [ text "Save" ]
