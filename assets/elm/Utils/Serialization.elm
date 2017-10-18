module Utils.Serialization exposing (listSingletonDecoder)

import Json.Decode as JD exposing (Decoder)


listSingletonDecoder : List a -> Decoder a
listSingletonDecoder list =
    case list of
        [ singleItem ] ->
            JD.succeed singleItem

        _ ->
            JD.fail "not singleton"
