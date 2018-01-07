module Data.Resource
    exposing
        ( Resource(..)
        , cancel
        , create
        , delete
        , load
        , new
        , remove
        , removeMultiple
        , save
        , unloaded
        , update
        )


type Resource k v
    = Loading
    | Create v
    | Read k v
    | Update k v v
    | Delete k v



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
