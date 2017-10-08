module Data.DataType exposing (DataType(..), toString)


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


toConfig : DataType -> DataTypeConfig
toConfig dataType =
    case dataType of
        Char size ->
            { dataType = Char size
            , string = ""
            }

        VarChar size ->
            { dataType = VarChar size
            , string = ""
            }

        Text ->
            { dataType = Text
            , string = ""
            }

        Bit size ->
            { dataType = Bit size
            , string = ""
            }

        VarBit size ->
            { dataType = VarBit size
            , string = ""
            }

        SmallInt ->
            { dataType = SmallInt
            , string = ""
            }

        Integer ->
            { dataType = Integer
            , string = ""
            }

        BigInt ->
            { dataType = BigInt
            , string = ""
            }

        SmallSerial ->
            { dataType = SmallSerial
            , string = ""
            }

        Serial ->
            { dataType = Serial
            , string = ""
            }

        BigSerial ->
            { dataType = BigSerial
            , string = ""
            }

        Numeric ( m, d ) ->
            { dataType = Numeric ( m, d )
            , string = ""
            }

        Double ->
            { dataType = Double
            , string = ""
            }

        Real ->
            { dataType = Real
            , string = ""
            }

        Money ->
            { dataType = Money
            , string = ""
            }

        Boolean ->
            { dataType = Boolean
            , string = ""
            }

        Date ->
            { dataType = Date
            , string = ""
            }

        TimeStamp withTimeZone ->
            { dataType = TimeStamp withTimeZone
            , string = ""
            }

        Time withTimeZone ->
            { dataType = Time withTimeZone
            , string = ""
            }


toString : DataType -> String
toString =
    toConfig >> .string
