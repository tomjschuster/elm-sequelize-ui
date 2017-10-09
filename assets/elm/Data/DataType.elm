module Data.DataType
    exposing
        ( DataType(..)
        , Modifier(..)
        , all
        , decoder
        , encode
        , encodeModifier
        , fromId
        , noModifier
        , none
        , toId
        , toInitialModifier
        , toString
        , updatePrecision
        , updateSize
        , updateWithTimezone
        )

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type DataType
    = None
    | Char
    | VarChar
    | Text
    | Bit
    | VarBit
    | SmallInt
    | Integer
    | BigInt
    | SmallSerial
    | Serial
    | BigSerial
    | Numeric
    | Double
    | Real
    | Money
    | Boolean
    | Date
    | TimeStamp
    | Time


all : List DataType
all =
    [ Char
    , VarChar
    , Text
    , Bit
    , VarBit
    , SmallInt
    , Integer
    , BigInt
    , SmallSerial
    , Serial
    , BigSerial
    , Numeric
    , Double
    , Real
    , Money
    , Boolean
    , Date
    , TimeStamp
    , Time
    ]


none : DataType
none =
    None



-- MODIFIERS


type Modifier
    = NoModifier
    | Size (Maybe Int)
    | Precision (Maybe Int) (Maybe Int)
    | WithTimezone Bool


noModifier : Modifier
noModifier =
    NoModifier


updateSize : Modifier -> Maybe Int -> Modifier
updateSize modifier size =
    case modifier of
        Size _ ->
            Size size

        _ ->
            modifier


updatePrecision : Modifier -> Maybe Int -> Maybe Int -> Modifier
updatePrecision modifier precision decimals =
    case modifier of
        Precision _ _ ->
            Precision precision decimals

        _ ->
            modifier


updateWithTimezone : Modifier -> Bool -> Modifier
updateWithTimezone modifier withTimezone =
    case modifier of
        WithTimezone _ ->
            WithTimezone withTimezone

        _ ->
            modifier



-- CONFIG


type alias DataTypeConfig =
    { dataType : DataType
    , id : Int
    , stringValue : String
    , initialModifier : Modifier
    }


toConfig : DataType -> DataTypeConfig
toConfig dataType =
    case dataType of
        None ->
            { dataType = None
            , id = 0
            , stringValue = "None"
            , initialModifier = NoModifier
            }

        Char ->
            { dataType = Char
            , id = 1
            , stringValue = "char"
            , initialModifier = Size Nothing
            }

        VarChar ->
            { dataType = VarChar
            , id = 2
            , stringValue = "varchar"
            , initialModifier = Size Nothing
            }

        Text ->
            { dataType = Text
            , id = 3
            , stringValue = "text"
            , initialModifier = NoModifier
            }

        Bit ->
            { dataType = Bit
            , id = 4
            , stringValue = "bit"
            , initialModifier = Size Nothing
            }

        VarBit ->
            { dataType = VarBit
            , id = 5
            , stringValue = "varbit"
            , initialModifier = Size Nothing
            }

        SmallInt ->
            { dataType = SmallInt
            , id = 6
            , stringValue = "smallint"
            , initialModifier = NoModifier
            }

        Integer ->
            { dataType = Integer
            , id = 7
            , stringValue = "int"
            , initialModifier = NoModifier
            }

        BigInt ->
            { dataType = BigInt
            , id = 8
            , stringValue = "bigint"
            , initialModifier = NoModifier
            }

        SmallSerial ->
            { dataType = SmallSerial
            , id = 9
            , stringValue = "smallserial"
            , initialModifier = NoModifier
            }

        Serial ->
            { dataType = Serial
            , id = 10
            , stringValue = "serial"
            , initialModifier = NoModifier
            }

        BigSerial ->
            { dataType = BigSerial
            , id = 11
            , stringValue = "bigserial"
            , initialModifier = NoModifier
            }

        Numeric ->
            { dataType = Numeric
            , id = 12
            , stringValue = "numeric"
            , initialModifier = Precision Nothing Nothing
            }

        Double ->
            { dataType = Double
            , id = 13
            , stringValue = "double"
            , initialModifier = NoModifier
            }

        Real ->
            { dataType = Real
            , id = 14
            , stringValue = "real"
            , initialModifier = NoModifier
            }

        Money ->
            { dataType = Money
            , id = 15
            , stringValue = "money"
            , initialModifier = NoModifier
            }

        Boolean ->
            { dataType = Boolean
            , id = 16
            , stringValue = "bool"
            , initialModifier = NoModifier
            }

        Date ->
            { dataType = Date
            , id = 17
            , stringValue = "date"
            , initialModifier = NoModifier
            }

        TimeStamp ->
            { dataType = TimeStamp
            , id = 18
            , stringValue = "timestamp"
            , initialModifier = WithTimezone False
            }

        Time ->
            { dataType = Time
            , id = 19
            , stringValue = "time"
            , initialModifier = WithTimezone False
            }


toId : DataType -> Int
toId =
    toConfig >> .id


toString : DataType -> String
toString =
    toConfig >> .stringValue


toInitialModifier : DataType -> Modifier
toInitialModifier =
    toConfig >> .initialModifier


fromId : Int -> Maybe DataType
fromId id =
    case id of
        1 ->
            Just Char

        2 ->
            Just VarChar

        3 ->
            Just Text

        4 ->
            Just Bit

        5 ->
            Just VarBit

        6 ->
            Just SmallInt

        7 ->
            Just Integer

        8 ->
            Just BigInt

        9 ->
            Just SmallSerial

        10 ->
            Just Serial

        11 ->
            Just BigSerial

        12 ->
            Just Numeric

        13 ->
            Just Double

        14 ->
            Just Real

        15 ->
            Just Money

        16 ->
            Just Boolean

        17 ->
            Just Date

        18 ->
            Just TimeStamp

        19 ->
            Just Time

        _ ->
            Nothing


decoder : Decoder DataType
decoder =
    JD.map (fromId >> Maybe.withDefault None) JD.int


encode : DataType -> Value
encode dataType =
    if dataType == None then
        JE.null
    else
        JE.int (toId dataType)


encodeModifier : Modifier -> List ( String, Value )
encodeModifier modifier =
    case modifier of
        NoModifier ->
            []

        Size size ->
            Debug.log "abc" [ ( "size", encodeNullableInt size ) ]

        Precision precision decimals ->
            [ ( "precision", encodeNullableInt precision )
            , ( "decimals", encodeNullableInt decimals )
            ]

        WithTimezone withTimezone ->
            [ ( "with_timezone", JE.bool withTimezone ) ]


encodeNullableInt : Maybe Int -> Value
encodeNullableInt =
    Maybe.map JE.int >> Maybe.withDefault JE.null
