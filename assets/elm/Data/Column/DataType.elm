module Data.Column.DataType
    exposing
        ( DataType(..)
        , all
        , bitStringGroup
        , booleanGroup
        , characterGroup
        , dateTimeGroup
        , decoder
        , defaultPrecision
        , defaultScale
        , defaultSize
        , encode
        , fromId
        , isMatch
        , isSameType
        , monetaryGroup
        , none
        , numericGroup
        , toId
        , toLongName
        , toShortName
        , toUrlParams
        )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Utils.Bool as BoolUtils
import Utils.Http as HttpUtils


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
    | Decimal (Maybe Int) (Maybe Int)
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
    [ char
    , varChar
    , text
    , smallInt
    , integer
    , bigInt
    , smallSerial
    , serial
    , bigSerial
    , decimal
    , double
    , real
    , boolean
    , bit
    , varBit
    , money
    , date
    , timeStamp
    , time
    ]


characterGroup : List DataType
characterGroup =
    [ char
    , varChar
    , text
    ]


numericGroup : List DataType
numericGroup =
    [ smallInt
    , integer
    , bigInt
    , smallSerial
    , serial
    , bigSerial
    , decimal
    , double
    , real
    ]


booleanGroup : List DataType
booleanGroup =
    [ boolean ]


bitStringGroup : List DataType
bitStringGroup =
    [ bit
    , varBit
    ]


monetaryGroup : List DataType
monetaryGroup =
    [ money ]


dateTimeGroup : List DataType
dateTimeGroup =
    [ date
    , timeStamp
    , time
    ]


none : DataType
none =
    NoDataType


char : DataType
char =
    Char Nothing


varChar : DataType
varChar =
    VarChar Nothing


text : DataType
text =
    Text


bit : DataType
bit =
    Bit Nothing


varBit : DataType
varBit =
    VarBit Nothing


smallInt : DataType
smallInt =
    SmallInt


integer : DataType
integer =
    Integer


bigInt : DataType
bigInt =
    BigInt


smallSerial : DataType
smallSerial =
    SmallSerial


serial : DataType
serial =
    Serial


bigSerial : DataType
bigSerial =
    BigSerial


decimal : DataType
decimal =
    Decimal Nothing Nothing


double : DataType
double =
    Double


real : DataType
real =
    Real


money : DataType
money =
    Money


boolean : DataType
boolean =
    Boolean


date : DataType
date =
    Date


timeStamp : DataType
timeStamp =
    TimeStamp False


time : DataType
time =
    Time False



--DEFAULTS


defaultSize : Int
defaultSize =
    255


sizeMatch : Maybe Int -> Maybe Int -> Bool
sizeMatch size1 size2 =
    Maybe.withDefault defaultSize size1 == Maybe.withDefault defaultSize size2


defaultPrecision : Int
defaultPrecision =
    18


precisionMatch : Maybe Int -> Maybe Int -> Bool
precisionMatch precision1 precision2 =
    Maybe.withDefault defaultPrecision precision1 == Maybe.withDefault defaultPrecision precision2


defaultScale : Int
defaultScale =
    0


scaleMatch : Maybe Int -> Maybe Int -> Bool
scaleMatch scale1 scale2 =
    Maybe.withDefault defaultScale scale1 == Maybe.withDefault defaultScale scale2


isMatch : DataType -> DataType -> Bool
isMatch dataType1 dataType2 =
    case dataType1 of
        NoDataType ->
            case dataType2 of
                NoDataType ->
                    True

                _ ->
                    False

        Char size1 ->
            case dataType2 of
                Char size2 ->
                    sizeMatch size1 size2

                _ ->
                    False

        VarChar size1 ->
            case dataType2 of
                VarChar size2 ->
                    sizeMatch size1 size2

                _ ->
                    False

        Text ->
            case dataType2 of
                Text ->
                    True

                _ ->
                    False

        Bit size1 ->
            case dataType2 of
                Bit size2 ->
                    sizeMatch size1 size2

                _ ->
                    False

        VarBit size1 ->
            case dataType2 of
                VarBit size2 ->
                    sizeMatch size1 size2

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

        Decimal precision1 scale1 ->
            case dataType2 of
                Decimal precision2 scale2 ->
                    precisionMatch precision1 precision2 && scaleMatch scale1 scale2

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

        TimeStamp withTimezone1 ->
            case dataType2 of
                TimeStamp withTimezone2 ->
                    withTimezone1 == withTimezone2

                _ ->
                    False

        Time withTimezone1 ->
            case dataType2 of
                Time withTimezone2 ->
                    withTimezone1 == withTimezone2

                _ ->
                    False


isSameType : DataType -> DataType -> Bool
isSameType dataType1 dataType2 =
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

        Decimal _ _ ->
            case dataType2 of
                Decimal _ _ ->
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

        Decimal precision scale ->
            { dataType = Decimal precision scale
            , id = 12
            , shortName = "decimal"
            , longName =
                "decimal ("
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
            Just char

        2 ->
            Just varChar

        3 ->
            Just text

        4 ->
            Just bit

        5 ->
            Just varBit

        6 ->
            Just smallInt

        7 ->
            Just integer

        8 ->
            Just bigInt

        9 ->
            Just smallSerial

        10 ->
            Just serial

        11 ->
            Just bigSerial

        12 ->
            Just decimal

        13 ->
            Just double

        14 ->
            Just real

        15 ->
            Just money

        16 ->
            Just boolean

        17 ->
            Just date

        18 ->
            Just timeStamp

        19 ->
            Just time

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

        Decimal precision scale ->
            precisionScaleDecoder Decimal

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


toUrlParams : DataType -> String
toUrlParams =
    toConfig >> configToParams >> HttpUtils.paramsToString


encodeConfig : DataTypeConfig -> List ( String, Value )
encodeConfig { dataType, id, size, precision, scale, withTimezone } =
    [ ( "data_type_id", encodeDataTypeId dataType )
    , ( "size", encodeNullableInt size )
    , ( "precision", encodeNullableInt precision )
    , ( "scale", encodeNullableInt scale )
    , ( "with_timezone", JE.bool withTimezone )
    ]


configToParams : DataTypeConfig -> List ( String, Maybe String )
configToParams { dataType, id, size, precision, scale, withTimezone } =
    [ ( "data_type_id", Just (toString id) )
    , ( "size", Maybe.map toString size )
    , ( "precision", Maybe.map toString precision )
    , ( "scale", Maybe.map toString scale )
    , ( "with_timezone", Just (BoolUtils.toString withTimezone) )
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
