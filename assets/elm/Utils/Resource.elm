module Utils.Resource
    exposing
        ( Error(..)
        , Resource(..)
        , cancel
        , create
        , delete
        , load
        , new
        , remove
        , removeMultiple
        , save
        , taskFromRequest
        , unloaded
        , update
        )

import Http
import Task exposing (Task)


type Resource k v
    = Loading
    | Create v
    | Read k v
    | Update k v v
    | Delete k v


type Error
    = InvalidAction
    | HttpError Http.Error



{- Build -}


unloaded : Resource k v
unloaded =
    Loading


new : v -> Resource k v
new value =
    Create value


load : comparable -> v -> Resource comparable v
load key value =
    Read key value



{- CRUD -}


create : comparable -> Resource comparable v -> Resource comparable v
create key resource =
    case resource of
        Create value ->
            Read key value

        _ ->
            resource


update : v -> Resource k v -> Resource k v
update value resource =
    case resource of
        Update key updated previous ->
            Update key value previous

        _ ->
            resource


save : Resource k v -> Resource k v
save resource =
    case resource of
        Update key updated previous ->
            Read key updated

        _ ->
            resource


cancel : Resource k v -> Resource k v
cancel resource =
    case resource of
        Update key updated previous ->
            Read key previous

        Delete key value ->
            Read key value

        _ ->
            resource


delete : Resource k v -> Resource k v
delete resource =
    case resource of
        Read key value ->
            Delete key value

        Update key updated previous ->
            Delete key previous

        _ ->
            resource


remove : k -> List (Resource k v) -> List (Resource k v)
remove key =
    List.filter (\x -> isDelete x && keyMatches key x)


removeMultiple : List k -> List (Resource k v) -> List (Resource k v)
removeMultiple keys =
    List.filter (\x -> isDelete x && List.any (flip keyMatches x) keys)



{- Query -}


getKey : Resource k v -> Maybe k
getKey resource =
    case resource of
        Loading ->
            Nothing

        Create _ ->
            Nothing

        Read key _ ->
            Just key

        Update key _ _ ->
            Just key

        Delete key _ ->
            Just key


keyMatches : k -> Resource k v -> Bool
keyMatches key =
    getKey >> Maybe.map ((==) key) >> Maybe.withDefault False


isLoading : Resource k v -> Bool
isLoading resource =
    case resource of
        Loading ->
            True

        _ ->
            False


isCreate : Resource k v -> Bool
isCreate resource =
    case resource of
        Create _ ->
            True

        _ ->
            False


isRead : Resource k v -> Bool
isRead resource =
    case resource of
        Read _ _ ->
            True

        _ ->
            False


isUpdate : Resource k v -> Bool
isUpdate resource =
    case resource of
        Update _ _ _ ->
            True

        _ ->
            False


isDelete : Resource k v -> Bool
isDelete resource =
    case resource of
        Delete _ _ ->
            True

        _ ->
            False



{- HTTP -}


taskFromRequest : Http.Request a -> Task Error a
taskFromRequest =
    Http.toTask >> Task.mapError HttpError



--module Request.Schema
--    exposing
--        ( create
--        , destroy
--        , index
--        , one
--        , resourceUrl
--        , update
--        , url
--        )
--import Data.Schema as Schema exposing (Schema)
--import Http
--import Json.Decode as JD
--import Json.Encode as JE
--import Task exposing (Task)
--import Utils.Http exposing (baseUrl, dataDecoder, delete, put)
--import Utils.Resource as Resource exposing (Resource(..))
--url : String
--url =
--    baseUrl ++ "schemas/"
--resourceUrl : Int -> String
--resourceUrl =
--    toString >> (++) url
--index : Http.Request (List Schema)
--index =
--    Http.get
--        url
--        (dataDecoder (JD.list Schema.decoder))
--one : Int -> Http.Request Schema
--one id =
--    Http.get (resourceUrl id) (dataDecoder Schema.decoder)
--create : Resource Int Schema -> Task Resource.Error Schema
--create resource =
--    case resource of
--        Create schema ->
--            Http.post
--                url
--                (JE.object [ ( "schema", Schema.encode schema ) ]
--                    |> Http.jsonBody
--                )
--                (dataDecoder Schema.decoder)
--                |> Resource.taskFromRequest
--        _ ->
--            Task.fail Resource.InvalidAction
--update : Resource Int Schema -> Task Resource.Error Schema
--update resource =
--    case resource of
--        Update id schema _ ->
--            put (resourceUrl id)
--                (JE.object [ ( "schema", Schema.encode schema ) ]
--                    |> Http.jsonBody
--                )
--                (dataDecoder Schema.decoder)
--                |> Http.toTask
--                |> Task.mapError Resource.HttpError
--        _ ->
--            Task.fail Resource.InvalidAction
--destroy : Int -> Http.Request ()
--destroy id =
--    delete (resourceUrl id)
