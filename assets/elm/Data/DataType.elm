module Data.DataType
    exposing
        ( DataType(..)
        , all
        , decoder
        , encode
        , fromId
        , none
        , toId
        , toString
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


none : DataType
none =
    None


type Option
    = NoOption
    | Size
    | Precision
    | TimeZone


type alias DataTypeConfig =
    { dataType : DataType
    , id : Int
    , stringValue : String
    , options : List Option
    }


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


toConfig : DataType -> DataTypeConfig
toConfig dataType =
    case dataType of
        None ->
            { dataType = None
            , id = 0
            , stringValue = "None"
            , options = []
            }

        Char ->
            { dataType = Char
            , id = 1
            , stringValue = "char"
            , options = [ Size ]
            }

        VarChar ->
            { dataType = VarChar
            , id = 2
            , stringValue = "varchar"
            , options = [ Size ]
            }

        Text ->
            { dataType = Text
            , id = 3
            , stringValue = "text"
            , options = []
            }

        Bit ->
            { dataType = Bit
            , id = 4
            , stringValue = "bit"
            , options = [ Size ]
            }

        VarBit ->
            { dataType = VarBit
            , id = 5
            , stringValue = "varbit"
            , options = [ Size ]
            }

        SmallInt ->
            { dataType = SmallInt
            , id = 6
            , stringValue = "smallint"
            , options = []
            }

        Integer ->
            { dataType = Integer
            , id = 7
            , stringValue = "int"
            , options = []
            }

        BigInt ->
            { dataType = BigInt
            , id = 8
            , stringValue = "bigint"
            , options = []
            }

        SmallSerial ->
            { dataType = SmallSerial
            , id = 9
            , stringValue = "smallserial"
            , options = []
            }

        Serial ->
            { dataType = Serial
            , id = 10
            , stringValue = "serial"
            , options = []
            }

        BigSerial ->
            { dataType = BigSerial
            , id = 11
            , stringValue = "bigserial"
            , options = []
            }

        Numeric ->
            { dataType = Numeric
            , id = 12
            , stringValue = "numeric"
            , options = [ Precision ]
            }

        Double ->
            { dataType = Double
            , id = 13
            , stringValue = "double"
            , options = []
            }

        Real ->
            { dataType = Real
            , id = 14
            , stringValue = "real"
            , options = []
            }

        Money ->
            { dataType = Money
            , id = 15
            , stringValue = "money"
            , options = []
            }

        Boolean ->
            { dataType = Boolean
            , id = 16
            , stringValue = "bool"
            , options = []
            }

        Date ->
            { dataType = Date
            , id = 17
            , stringValue = "date"
            , options = []
            }

        TimeStamp ->
            { dataType = TimeStamp
            , id = 18
            , stringValue = "timestamp"
            , options = [ TimeZone ]
            }

        Time ->
            { dataType = Time
            , id = 19
            , stringValue = "time"
            , options = [ TimeZone ]
            }


toId : DataType -> Int
toId =
    toConfig >> .id


toString : DataType -> String
toString =
    toConfig >> .stringValue


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
