module Utils.List
    exposing
        ( add
        , find
        , findWithDefault
        , groupBy
        , groupByMap
        , maybeAdd
        , maybeCons
        , replaceIfMatch
        , toDictBy
        )

import Dict exposing (Dict)


toDictBy : (a -> comparable) -> List a -> Dict comparable a
toDictBy toKey =
    List.foldl (\v acc -> Dict.insert (toKey v) v acc) Dict.empty


groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy toKey =
    List.foldr
        (\x -> Dict.update (toKey x) (Maybe.withDefault [] >> (::) x >> Just))
        Dict.empty


groupByMap : (a -> comparable) -> (a -> b) -> List a -> Dict comparable (List b)
groupByMap toKey f =
    List.foldr
        (\x -> Dict.update (toKey x) (Maybe.withDefault [] >> (::) (f x) >> Just))
        Dict.empty


replaceIfMatch : (a -> id) -> a -> List a -> List a
replaceIfMatch toId item =
    List.map
        (\currItem ->
            if toId currItem == toId item then
                item
            else
                currItem
        )


add : a -> List a -> List a
add item =
    List.foldr (::) [ item ]


maybeCons : Maybe a -> List a -> List a
maybeCons maybe list =
    maybe |> Maybe.map (flip (::) list) |> Maybe.withDefault list


maybeAdd : Maybe a -> List a -> List a
maybeAdd maybe list =
    maybe |> Maybe.map (flip add list) |> Maybe.withDefault list


find : (a -> Bool) -> List a -> Maybe a
find f =
    List.filter f >> List.head


findWithDefault : a -> (a -> Bool) -> List a -> a
findWithDefault default f =
    find f >> Maybe.withDefault default
