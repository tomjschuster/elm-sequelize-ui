module Data.DataType
    exposing
        ( DataType(..)
        , defaultList
        , toString
        )


type DataType
    = Char Int
    | VarChar Int
    | Text
    | Bit Int
    | VarBit Int
    | SmallInt
    | Integer
    | BigInt
    | SmallSerial
    | Serial
    | BigSerial
    | Numeric ( Int, Int )
    | Double
    | Real
    | Money
    | Boolean
    | Date
    | TimeStamp Bool
    | Time Bool


type alias DataTypeConfig =
    { dataType : DataType
    , string : String
    }


defaultList : List DataType
defaultList =
    [ Char 255
    , VarChar 255
    , Text
    , Bit 255
    , VarBit 255
    , SmallInt
    , Integer
    , BigInt
    , SmallSerial
    , Serial
    , BigSerial
    , Numeric ( 52, 2 )
    , Double
    , Real
    , Money
    , Boolean
    , Date
    , TimeStamp False
    , Time False
    ]


toConfig : DataType -> DataTypeConfig
toConfig dataType =
    case dataType of
        Char size ->
            { dataType = Char size
            , string = "char"
            }

        VarChar size ->
            { dataType = VarChar size
            , string = "varchar"
            }

        Text ->
            { dataType = Text
            , string = "text"
            }

        Bit size ->
            { dataType = Bit size
            , string = "bit"
            }

        VarBit size ->
            { dataType = VarBit size
            , string = "varbit"
            }

        SmallInt ->
            { dataType = SmallInt
            , string = "smallint"
            }

        Integer ->
            { dataType = Integer
            , string = "int"
            }

        BigInt ->
            { dataType = BigInt
            , string = "bigint"
            }

        SmallSerial ->
            { dataType = SmallSerial
            , string = "smallserial"
            }

        Serial ->
            { dataType = Serial
            , string = "serial"
            }

        BigSerial ->
            { dataType = BigSerial
            , string = "bigserial"
            }

        Numeric ( m, d ) ->
            { dataType = Numeric ( m, d )
            , string = "numeric"
            }

        Double ->
            { dataType = Double
            , string = "double"
            }

        Real ->
            { dataType = Real
            , string = "real"
            }

        Money ->
            { dataType = Money
            , string = "money"
            }

        Boolean ->
            { dataType = Boolean
            , string = "bool"
            }

        Date ->
            { dataType = Date
            , string = "date"
            }

        TimeStamp withTimeZone ->
            { dataType = TimeStamp withTimeZone
            , string = "timestamp"
            }

        Time withTimeZone ->
            { dataType = Time withTimeZone
            , string = "time"
            }


toString : DataType -> String
toString =
    toConfig >> .string
