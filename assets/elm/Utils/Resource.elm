module Utils.Resource
    exposing
        ( Config
        , Error(..)
        , Resource(..)
        , build
        , cancel
        , clearDestroyed
        , create
        , delete
        , destroy
        , load
        , none
        , save
        , saveAndContinue
        , taskFromRequest
        , update
        )

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Task exposing (Task)


type Resource id data
    = None
    | Requested id
    | Create data
    | Read id data
    | Update id data data
    | Delete id data
    | Destroyed id


type alias Config id data =
    { baseUrl : String
    , decodeAt : List String
    , idDecoder : Decoder id
    , dataDecoder : Decoder data
    , idToString : id -> String
    , encode : Maybe id -> Maybe data -> Value
    }


type Error
    = InvalidAction
    | HttpError Http.Error



{- Build -}


none : Resource id data
none =
    None


build : data -> Resource id data
build data =
    Create data


read : id -> data -> Resource id data
read id data =
    Read id data



{- Query -}


getId : Resource id data -> Maybe id
getId resource =
    case resource of
        None ->
            Nothing

        Requested id ->
            Just id

        Create _ ->
            Nothing

        Read id _ ->
            Just id

        Update id _ _ ->
            Just id

        Delete id _ ->
            Just id

        Destroyed id ->
            Just id


getData : Resource id data -> Maybe data
getData resource =
    case resource of
        None ->
            Nothing

        Requested _ ->
            Nothing

        Create data ->
            Just data

        Read _ data ->
            Just data

        Update _ data _ ->
            Just data

        Delete _ data ->
            Just data

        Destroyed _ ->
            Nothing


idMatches : id -> Resource id data -> Bool
idMatches id =
    getId >> Maybe.map ((==) id) >> Maybe.withDefault False


isNone : Resource id data -> Bool
isNone resource =
    case resource of
        None ->
            True

        _ ->
            False


isRequested : Resource id data -> Bool
isRequested resource =
    case resource of
        Requested _ ->
            True

        _ ->
            False


isCreate : Resource id data -> Bool
isCreate resource =
    case resource of
        Create _ ->
            True

        _ ->
            False


isRead : Resource id data -> Bool
isRead resource =
    case resource of
        Read _ _ ->
            True

        _ ->
            False


isUpdate : Resource id data -> Bool
isUpdate resource =
    case resource of
        Update _ _ _ ->
            True

        _ ->
            False


isDelete : Resource id data -> Bool
isDelete resource =
    case resource of
        Delete _ _ ->
            True

        _ ->
            False


isDestroyed : Resource id data -> Bool
isDestroyed resource =
    case resource of
        Destroyed _ ->
            True

        _ ->
            False



{- Edit -}


update : data -> Resource id data -> Resource id data
update data resource =
    case resource of
        Update id updated previous ->
            Update id data previous

        _ ->
            resource


cancel : Resource id data -> Resource id data
cancel resource =
    case resource of
        Update id updated previous ->
            Read id previous

        Delete id data ->
            Read id data

        _ ->
            resource


delete : Resource id data -> Resource id data
delete resource =
    case resource of
        Read id data ->
            Delete id data

        Update id updated previous ->
            Delete id previous

        _ ->
            resource


clearDestroyed : List (Resource id data) -> List (Resource id data)
clearDestroyed =
    List.filter (not << isDestroyed)



{- Persist -}


create : Config id data -> Resource id data -> Task Error (Resource id data)
create config resource =
    case resource of
        Create data ->
            Http.post
                config.baseUrl
                (encodeToJson config resource)
                (readDecoder config)
                |> taskFromRequest

        _ ->
            Task.fail InvalidAction


load : Config id data -> Resource id data -> Task Error (Resource id data)
load config resource =
    case resource of
        Requested id ->
            Http.get
                (resourceUrl config id)
                (readDecoder config)
                |> taskFromRequest

        _ ->
            Task.fail InvalidAction


loadAll : Config id data -> Task Error (List (Resource id data))
loadAll config =
    Http.get
        config.baseUrl
        (readListDecoder config)
        |> taskFromRequest


save : Config id data -> Resource id data -> Task Error (Resource id data)
save config resource =
    case resource of
        Update id data previous ->
            Http.post
                config.baseUrl
                (encodeToJson config resource)
                (readDecoder config)
                |> taskFromRequest

        _ ->
            Task.fail InvalidAction


saveAndContinue : Config id data -> Resource id data -> Task Error (Resource id data)
saveAndContinue config resource =
    case resource of
        Update id data previous ->
            putRequest
                config.baseUrl
                (encodeToJson config resource)
                (updateDecoder config)
                |> taskFromRequest

        _ ->
            Task.fail InvalidAction


destroy : Config id data -> Resource id data -> Task Error (Resource id data)
destroy config resource =
    case resource of
        Delete id data ->
            deleteRequest (resourceUrl config id)
                |> taskFromRequest
                |> Task.map (always (Destroyed id))

        _ ->
            Task.fail InvalidAction



-- Http Helpers


resourceUrl : Config id data -> id -> String
resourceUrl { baseUrl, idToString } id =
    baseUrl ++ idToString id


readDecoder : Config id data -> Decoder (Resource id data)
readDecoder { decodeAt, idDecoder, dataDecoder } =
    JD.at decodeAt <| JD.map2 Read idDecoder dataDecoder


readListDecoder : Config id data -> Decoder (List (Resource id data))
readListDecoder { decodeAt, idDecoder, dataDecoder } =
    JD.at decodeAt <| JD.list <| JD.map2 Read idDecoder dataDecoder


updateDecoder : Config id data -> Decoder (Resource id data)
updateDecoder { decodeAt, idDecoder, dataDecoder } =
    JD.at decodeAt <| JD.map3 Update idDecoder dataDecoder dataDecoder


encodeToJson : Config id data -> Resource id data -> Http.Body
encodeToJson { encode } resource =
    encode (getId resource) (getData resource)
        |> Http.jsonBody


taskFromRequest : Http.Request a -> Task Error a
taskFromRequest =
    Http.toTask >> Task.mapError HttpError


putRequest : String -> Http.Body -> JD.Decoder a -> Http.Request a
putRequest url body decoder =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


deleteRequest : String -> Http.Request ()
deleteRequest url =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (always (Ok ()))
        , timeout = Nothing
        , withCredentials = False
        }
