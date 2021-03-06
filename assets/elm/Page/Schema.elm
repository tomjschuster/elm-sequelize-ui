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
        , section
        , text
        , ul
        )
import Html.Attributes as Attr
import Html.Events as Evt
import Http
import Request.Schema as SchemaReq
import Request.Table as TableReq
import Router exposing (Route)
import Task
import Utils.Events as EvtUtils
import Utils.Keys exposing (Key(..))
import Views.Breadcrumbs as BreadCrumbs
import Views.ChangesetError as CE


-- MODEL


type alias Model =
    { schema : Schema
    , tables : List Table
    , editingSchema : Maybe Schema
    , editing : Bool
    , newTable : Table
    , editingTable : Maybe Table
    , toDeleteId : Maybe Int
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    Model Schema.empty [] Nothing False Table.empty Nothing Nothing []


init : Int -> ( Model, Cmd Msg )
init schemaId =
    ( { initialModel | newTable = Table.init schemaId }
    , Cmd.batch
        [ SchemaReq.one schemaId |> Http.send LoadSchema
        , TableReq.indexForSchema schemaId |> Http.send LoadTables
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | FocusResult (Result Dom.Error ())
    | Goto Route
      -- SCHEMA
    | LoadSchema (Result Http.Error Schema)
    | EditSchema
    | InputSchemaName String
    | CancelEditSchema
    | SaveSchema
    | Destroy
    | RemoveSchema (Result Http.Error ())
      -- TABLES
    | LoadTables (Result Http.Error (List Table))
    | InputNewTableName String
    | CreateTable
    | LoadTable (Result Http.Error Table)
    | EditTableName Int
    | InputEditingTableName String
    | CancelEditTableName
    | SaveTableName
    | UpdateTable (Result Http.Error Table)
    | DestroyTable Int
    | RemoveTable (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg, AppUpdate )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, AppUpdate.none )

        FocusResult _ ->
            ( model, Cmd.none, AppUpdate.none )

        Goto route ->
            ( model
            , Router.goto route
            , AppUpdate.none
            )

        -- SCHEMA
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
                | editingTable = Nothing
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
                |> Maybe.map (SchemaReq.update >> Http.send LoadSchema)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        Destroy ->
            ( model
            , SchemaReq.destroy model.schema.id |> Http.send RemoveSchema
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

        -- TABLES
        LoadTables (Ok tables) ->
            ( { model | tables = tables }, Cmd.none, AppUpdate.none )

        LoadTables (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        InputNewTableName name ->
            ( { model | newTable = Table.updateName name model.newTable }
            , Cmd.none
            , AppUpdate.none
            )

        CreateTable ->
            ( model
            , TableReq.create model.newTable
                |> Http.send LoadTable
            , AppUpdate.none
            )

        LoadTable (Ok table) ->
            ( { model
                | tables = model.tables ++ [ table ]
                , newTable = Table.init model.schema.id
                , errors = []
              }
            , Dom.focus "create-table" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        LoadTable (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Dom.focus "create-table" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        EditTableName id ->
            ( { model
                | editingSchema = Nothing
                , editingTable =
                    model.tables |> List.filter (.id >> (==) id) >> List.head
              }
            , Dom.focus "edit-table-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        InputEditingTableName name ->
            ( { model
                | editingTable =
                    Maybe.map (Table.updateName name) model.editingTable
              }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditTableName ->
            ( { model | editingTable = Nothing }
            , Cmd.none
            , AppUpdate.none
            )

        SaveTableName ->
            ( model
            , model.editingTable
                |> Maybe.map (TableReq.update >> Http.send UpdateTable)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        UpdateTable (Ok table) ->
            ( { model
                | tables =
                    List.map (Table.replaceIfMatch table) model.tables
                , editingTable = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateTable (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Dom.focus "edit-table-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        DestroyTable id ->
            ( { model | toDeleteId = Just id }
            , TableReq.destroy id
                |> Http.send RemoveTable
            , AppUpdate.none
            )

        RemoveTable (Ok ()) ->
            ( { model
                | tables =
                    List.filter
                        (.id >> Just >> (/=) model.toDeleteId)
                        model.tables
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        RemoveTable (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
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
        [ breadCrumbs model.schema
        , schemaView model
        , tablesView model
        ]


breadCrumbs : Schema -> Html Msg
breadCrumbs schema =
    BreadCrumbs.view Goto
        [ BreadCrumbs.home, BreadCrumbs.schema schema ]



-- SCHEMA VIEW


schemaView : Model -> Html Msg
schemaView { editingSchema, schema } =
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
    button [ Evt.onClick SaveSchema ] [ text "Save" ]


cancelEditSchemaButton : Html Msg
cancelEditSchemaButton =
    button [ Evt.onClick CancelEditSchema ] [ text "Cancel" ]


editSchemaButton : Html Msg
editSchemaButton =
    button [ Evt.onClick EditSchema ] [ text "Edit Name" ]


deleteSchemaButton : Html Msg
deleteSchemaButton =
    button [ Evt.onClick Destroy ] [ text "Delete Schema" ]



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
        [ Attr.id "edit-schema-name"
        , Attr.value name
        , Evt.onInput InputSchemaName
        , EvtUtils.customOnKeyDown onSchemaNameKeyDown
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



-- TABLES VIEW


tablesView : Model -> Html Msg
tablesView model =
    section [] (tablesChildren model)


tablesChildren : Model -> List (Html Msg)
tablesChildren { editingTable, tables, newTable, errors } =
    CE.prependIfErrors
        errors
        [ tablesTitle
        , createTableView newTable.name
        , tablesListView editingTable tables
        ]


tablesTitle : Html msg
tablesTitle =
    h3 [] [ text "Tables" ]


createTableView : String -> Html Msg
createTableView name =
    div []
        [ input
            [ Attr.id "create-table"
            , Attr.value name
            , Evt.onInput InputNewTableName
            , EvtUtils.onEnter CreateTable
            ]
            []
        , button
            [ Evt.onClick CreateTable
            ]
            [ text "Create Table" ]
        ]


tablesListView : Maybe Table -> List Table -> Html Msg
tablesListView editingTable tables =
    ul [] (List.map (tableView editingTable) tables)


tableView : Maybe Table -> Table -> Html Msg
tableView editingTable table =
    li [] (tableChildrenView table editingTable)


tableChildrenView : Table -> Maybe Table -> List (Html Msg)
tableChildrenView table =
    Maybe.map (getEditingTableChildren table)
        >> Maybe.withDefault (normalTableChildren table)


getEditingTableChildren : Table -> Table -> List (Html Msg)
getEditingTableChildren table editingTable =
    if editingTable.id == table.id then
        editingTableChildren editingTable
    else
        [ text table.name ]


normalTableChildren : Table -> List (Html Msg)
normalTableChildren table =
    [ tableLink table
    , editTableNameButton table.id
    , deleteTableButton table.id
    ]


editingTableChildren : Table -> List (Html Msg)
editingTableChildren { name } =
    [ editTableNameInput name
    , cancelEditTableNameButton
    , saveTableNameButton
    ]


tableLink : Table -> Html Msg
tableLink { id, name, schemaId } =
    Router.link Goto (Router.Table schemaId id) [] [ text name ]


editTableNameButton : Int -> Html Msg
editTableNameButton id =
    button [ Evt.onClick (EditTableName id) ] [ text "Edit Name" ]


deleteTableButton : Int -> Html Msg
deleteTableButton id =
    button [ Evt.onClick (DestroyTable id) ] [ text "Delete" ]


editTableNameInput : String -> Html Msg
editTableNameInput name =
    input
        [ Attr.id "edit-table-name"
        , Attr.value name
        , Evt.onInput InputEditingTableName
        , EvtUtils.customOnKeyDown onTableNameKeyDown
        ]
        []


onTableNameKeyDown : Key -> Maybe Msg
onTableNameKeyDown key =
    case key of
        Enter ->
            Just SaveTableName

        Escape ->
            Just CancelEditTableName

        _ ->
            Nothing


cancelEditTableNameButton : Html Msg
cancelEditTableNameButton =
    button [ Evt.onClick CancelEditTableName ] [ text "Cancel" ]


saveTableNameButton : Html Msg
saveTableNameButton =
    button [ Evt.onClick SaveTableName ] [ text "Save" ]
