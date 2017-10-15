module Data.DataType
    exposing
        ( DataType(..)
        , all
        , decoder
        , defaultPrecision
        , defaultScale
        , defaultSize
        , encode
        , fromId
        , isSame
        , none
        , toId
        , toLongName
        , toShortName
        )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)


type DataType
    = NoDataType
    | Char (Maybe Int)
    | VarChar (Maybe Int)
    | Text
    | Bit (Maybe Int)
    | VarBit (Maybe Int)
    | SmallInt
    | Integer
    | BigInt
    | SmallSerial
    | Serial
    | BigSerial
    | Numeric (Maybe Int) (Maybe Int)
    | Double
    | Real
    | Money
    | Boolean
    | Date
    | TimeStamp Bool
    | Time Bool



-- INITIAL DATATYPES


all : List DataType
all =
    [ emptyChar
    , emptyVarChar
    , emptyText
    , emptyBit
    , emptyVarBit
    , emptySmallInt
    , emptyInteger
    , emptyBigInt
    , emptySmallSerial
    , emptySerial
    , emptyBigSerial
    , emptyNumeric
    , emptyDouble
    , emptyReal
    , emptyMoney
    , emptyBoolean
    , emptyDate
    , emptyTimeStamp
    , emptyTime
    ]


none : DataType
none =
    NoDataType


emptyChar : DataType
emptyChar =
    Char Nothing


emptyVarChar : DataType
emptyVarChar =
    VarChar Nothing


emptyText : DataType
emptyText =
    Text


emptyBit : DataType
emptyBit =
    Bit Nothing


emptyVarBit : DataType
emptyVarBit =
    VarBit Nothing


emptySmallInt : DataType
emptySmallInt =
    SmallInt


emptyInteger : DataType
emptyInteger =
    Integer


emptyBigInt : DataType
emptyBigInt =
    BigInt


emptySmallSerial : DataType
emptySmallSerial =
    SmallSerial


emptySerial : DataType
emptySerial =
    Serial


emptyBigSerial : DataType
emptyBigSerial =
    BigSerial


emptyNumeric : DataType
emptyNumeric =
    Numeric Nothing Nothing


emptyDouble : DataType
emptyDouble =
    Double


emptyReal : DataType
emptyReal =
    Real


emptyMoney : DataType
emptyMoney =
    Money


emptyBoolean : DataType
emptyBoolean =
    Boolean


emptyDate : DataType
emptyDate =
    Date


emptyTimeStamp : DataType
emptyTimeStamp =
    TimeStamp False


emptyTime : DataType
emptyTime =
    Time False



--DEFAULTS


defaultSize : Int
defaultSize =
    255


defaultPrecision : Int
defaultPrecision =
    18


defaultScale : Int
defaultScale =
    0


isSame : DataType -> DataType -> Bool
isSame dataType1 dataType2 =
    case dataType1 of
        NoDataType ->
            case dataType2 of
                NoDataType ->
                    True

                _ ->
                    False

        Char _ ->
            case dataType2 of
                Char _ ->
                    True

                _ ->
                    False

        VarChar _ ->
            case dataType2 of
                VarChar _ ->
                    True

                _ ->
                    False

        Text ->
            case dataType2 of
                Text ->
                    True

                _ ->
                    False

        Bit _ ->
            case dataType2 of
                Bit _ ->
                    True

                _ ->
                    False

        VarBit _ ->
            case dataType2 of
                VarBit _ ->
                    True

                _ ->
                    False

        SmallInt ->
            case dataType2 of
                SmallInt ->
                    True

                _ ->
                    False

        Integer ->
            case dataType2 of
                Integer ->
                    True

                _ ->
                    False

        BigInt ->
            case dataType2 of
                BigInt ->
                    True

                _ ->
                    False

        SmallSerial ->
            case dataType2 of
                SmallSerial ->
                    True

                _ ->
                    False

        Serial ->
            case dataType2 of
                Serial ->
                    True

                _ ->
                    False

        BigSerial ->
            case dataType2 of
                BigSerial ->
                    True

                _ ->
                    False

        Numeric _ _ ->
            case dataType2 of
                Numeric _ _ ->
                    True

                _ ->
                    False

        Double ->
            case dataType2 of
                Double ->
                    True

                _ ->
                    False

        Real ->
            case dataType2 of
                Real ->
                    True

                _ ->
                    False

        Money ->
            case dataType2 of
                Money ->
                    True

                _ ->
                    False

        Boolean ->
            case dataType2 of
                Boolean ->
                    True

                _ ->
                    False

        Date ->
            case dataType2 of
                Date ->
                    True

                _ ->
                    False

        TimeStamp _ ->
            case dataType2 of
                TimeStamp _ ->
                    True

                _ ->
                    False

        Time _ ->
            case dataType2 of
                Time _ ->
                    True

                _ ->
                    False



-- CONFIG


type alias DataTypeConfig =
    { dataType : DataType
    , id : Int
    , shortName : String
    , longName : String
    , size : Maybe Int
    , precision : Maybe Int
    , scale : Maybe Int
    , withTimezone : Bool
    }


toConfig : DataType -> DataTypeConfig
toConfig dataType =
    case dataType of
        NoDataType ->
            { dataType = NoDataType
            , id = 0
            , shortName = ""
            , longName = ""
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        Char size ->
            { dataType = Char size
            , id = 1
            , shortName = "char"
            , longName = "char (" ++ displaySize size ++ ")"
            , size = Just <| Maybe.withDefault defaultSize size
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        VarChar size ->
            { dataType = VarChar size
            , id = 2
            , shortName = "varchar"
            , longName = "varchar (" ++ displaySize size ++ ")"
            , size = Just <| Maybe.withDefault defaultSize size
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        Text ->
            { dataType = Text
            , id = 3
            , shortName = "text"
            , longName = "text"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        Bit size ->
            { dataType = Bit size
            , id = 4
            , shortName = "bit"
            , longName = "bit (" ++ displaySize size ++ ")"
            , size = Just <| Maybe.withDefault defaultSize size
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        VarBit size ->
            { dataType = VarBit size
            , id = 5
            , shortName = "varbit"
            , longName = "varbit (" ++ displaySize size ++ ")"
            , size = Just <| Maybe.withDefault defaultSize size
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        SmallInt ->
            { dataType = SmallInt
            , id = 6
            , shortName = "smallint"
            , longName = "smallint"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        Integer ->
            { dataType = Integer
            , id = 7
            , shortName = "int"
            , longName = "int"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        BigInt ->
            { dataType = BigInt
            , id = 8
            , shortName = "bigint"
            , longName = "bigint"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        SmallSerial ->
            { dataType = SmallSerial
            , id = 9
            , shortName = "smallserial"
            , longName = "smallserial"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        Serial ->
            { dataType = Serial
            , id = 10
            , shortName = "serial"
            , longName = "serial"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        BigSerial ->
            { dataType = BigSerial
            , id = 11
            , shortName = "bigserial"
            , longName = "bigserial"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        Numeric precision scale ->
            { dataType = Numeric precision scale
            , id = 12
            , shortName = "numeric"
            , longName =
                "numeric ("
                    ++ displayPrecision precision
                    ++ ", "
                    ++ displayScale scale
                    ++ ")"
            , size = Nothing
            , precision = Just <| Maybe.withDefault defaultPrecision precision
            , scale = Just <| Maybe.withDefault defaultScale scale
            , withTimezone = False
            }

        Double ->
            { dataType = Double
            , id = 13
            , shortName = "double"
            , longName = "double"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        Real ->
            { dataType = Real
            , id = 14
            , shortName = "real"
            , longName = "real"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        Money ->
            { dataType = Money
            , id = 15
            , shortName = "money"
            , longName = "money"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        Boolean ->
            { dataType = Boolean
            , id = 16
            , shortName = "bool"
            , longName = "bool"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        Date ->
            { dataType = Date
            , id = 17
            , shortName = "date"
            , longName = "date"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = False
            }

        TimeStamp withTimezone ->
            { dataType = TimeStamp withTimezone
            , id = 18
            , shortName = "timestamp"
            , longName =
                if withTimezone then
                    "timestamp with timezone"
                else
                    "timestamp without timezone"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = withTimezone
            }

        Time withTimezone ->
            { dataType = Time withTimezone
            , id = 19
            , shortName = "time"
            , longName =
                if withTimezone then
                    "time with timezone"
                else
                    "time without timezone"
            , size = Nothing
            , precision = Nothing
            , scale = Nothing
            , withTimezone = withTimezone
            }



-- HELPERS


displaySize : Maybe Int -> String
displaySize =
    Maybe.withDefault defaultSize >> toString


displayPrecision : Maybe Int -> String
displayPrecision =
    Maybe.withDefault defaultPrecision >> toString


displayScale : Maybe Int -> String
displayScale =
    Maybe.withDefault defaultScale >> toString


toId : DataType -> Int
toId =
    toConfig >> .id


toShortName : DataType -> String
toShortName =
    toConfig >> .shortName


toLongName : DataType -> String
toLongName =
    toConfig >> .longName


toSize : DataType -> Maybe Int
toSize =
    toConfig >> .size


toPrecision : DataType -> Maybe Int
toPrecision =
    toConfig >> .precision


toScale : DataType -> Maybe Int
toScale =
    toConfig >> .scale


toWithTimezone : DataType -> Bool
toWithTimezone =
    toConfig >> .withTimezone


fromId : Int -> Maybe DataType
fromId id =
    case id of
        1 ->
            Just emptyChar

        2 ->
            Just emptyVarChar

        3 ->
            Just emptyText

        4 ->
            Just emptyBit

        5 ->
            Just emptyVarBit

        6 ->
            Just emptySmallInt

        7 ->
            Just emptyInteger

        8 ->
            Just emptyBigInt

        9 ->
            Just emptySmallSerial

        10 ->
            Just emptySerial

        11 ->
            Just emptyBigSerial

        12 ->
            Just emptyNumeric

        13 ->
            Just emptyDouble

        14 ->
            Just emptyReal

        15 ->
            Just emptyMoney

        16 ->
            Just emptyBoolean

        17 ->
            Just emptyDate

        18 ->
            Just emptyTimeStamp

        19 ->
            Just emptyTime

        _ ->
            Nothing



-- DECODERS


decoder : Decoder DataType
decoder =
    JD.field "dataTypeId" JD.int
        |> JD.andThen
            (fromId >> Maybe.withDefault NoDataType >> dataTypeDecoder)


dataTypeDecoder : DataType -> Decoder DataType
dataTypeDecoder dataType =
    case dataType of
        Char size ->
            sizeDecoder Char

        VarChar size ->
            sizeDecoder VarChar

        Bit size ->
            sizeDecoder Bit

        VarBit size ->
            sizeDecoder VarBit

        Numeric precision scale ->
            precisionScaleDecoder Numeric

        TimeStamp withTimezone ->
            withTimezoneDecoder TimeStamp

        Time withTimezone ->
            withTimezoneDecoder Time

        otherType ->
            JD.succeed otherType


sizeDecoder : (Maybe Int -> DataType) -> Decoder DataType
sizeDecoder toDataType =
    JDP.decode toDataType
        |> JDP.required "size" (JD.maybe JD.int)


precisionScaleDecoder : (Maybe Int -> Maybe Int -> DataType) -> Decoder DataType
precisionScaleDecoder toDataType =
    JDP.decode toDataType
        |> JDP.required "precision" (JD.maybe JD.int)
        |> JDP.required "scale" (JD.maybe JD.int)


withTimezoneDecoder : (Bool -> DataType) -> Decoder DataType
withTimezoneDecoder toDataType =
    JDP.decode toDataType
        |> JDP.required "withTimezone" JD.bool



-- ENCODERS


encode : DataType -> List ( String, Value )
encode =
    toConfig >> encodeConfig


encodeConfig : DataTypeConfig -> List ( String, Value )
encodeConfig { dataType, id, size, precision, scale, withTimezone } =
    [ ( "data_type_id", encodeDataTypeId dataType )
    , ( "size", encodeNullableInt size )
    , ( "precision", encodeNullableInt precision )
    , ( "scale", encodeNullableInt scale )
    , ( "with_timezone", JE.bool withTimezone )
    ]


encodeDataTypeId : DataType -> Value
encodeDataTypeId dataType =
    if dataType == NoDataType then
        JE.null
    else
        JE.int (toId dataType)


encodeNullableInt : Maybe Int -> Value
encodeNullableInt =
    Maybe.map JE.int >> Maybe.withDefault JE.null
