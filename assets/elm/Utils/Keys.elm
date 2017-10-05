module Utils.Keys exposing (Key(..), fromKeyCode, toKeyCode)


type Key
    = Backspace
    | Tab
    | Enter
    | Shift
    | Ctrl
    | Alt
    | Escape
    | Spacebar
    | PageUp
    | PageDown
    | End
    | Home
    | LeftArrow
    | UpArrow
    | RightArrow
    | DownArrow
    | Insert
    | Delete
    | MetaLeft
    | MetaRight


toKeyCode : Key -> Int
toKeyCode key =
    case key of
        Backspace ->
            8

        Tab ->
            9

        Enter ->
            13

        Shift ->
            16

        Ctrl ->
            17

        Alt ->
            18

        Escape ->
            27

        Spacebar ->
            32

        PageUp ->
            33

        PageDown ->
            34

        End ->
            35

        Home ->
            36

        LeftArrow ->
            37

        UpArrow ->
            38

        RightArrow ->
            39

        DownArrow ->
            40

        Insert ->
            45

        Delete ->
            46

        MetaLeft ->
            91

        MetaRight ->
            93


fromKeyCode : Int -> Maybe Key
fromKeyCode keyCode =
    case keyCode of
        8 ->
            Just Backspace

        9 ->
            Just Tab

        13 ->
            Just Enter

        16 ->
            Just Shift

        17 ->
            Just Ctrl

        18 ->
            Just Alt

        27 ->
            Just Escape

        32 ->
            Just Spacebar

        33 ->
            Just PageUp

        34 ->
            Just PageDown

        35 ->
            Just End

        36 ->
            Just Home

        37 ->
            Just LeftArrow

        38 ->
            Just UpArrow

        39 ->
            Just RightArrow

        40 ->
            Just DownArrow

        45 ->
            Just Insert

        46 ->
            Just Delete

        91 ->
            Just MetaLeft

        93 ->
            Just MetaRight

        _ ->
            Nothing
