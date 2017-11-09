module Utils.Bool exposing (toString)


toString : Bool -> String
toString bool =
    case bool of
        True ->
            "true"

        False ->
            "false"
