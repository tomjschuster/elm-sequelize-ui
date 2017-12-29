module Utils.List exposing (add, replaceIfMatch, toLookup)

import Dict exposing (Dict)


toLookup : (a -> comparable) -> List a -> Dict comparable a
toLookup toKey =
    List.map (\v -> ( toKey v, v )) >> Dict.fromList


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
