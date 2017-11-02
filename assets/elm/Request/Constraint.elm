module Request.Constraint
    exposing
        ( destroy
        , indexForTable
        )

import Data.Constraints as Constraints exposing (TableConstraints)
import Http exposing (Request)
import Request.Table exposing (tableUrl)
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


constraintsUrl : String
constraintsUrl =
    baseUrl ++ "constraints/"


columnUrl : Int -> String
columnUrl =
    toString >> (++) constraintsUrl


tableConstraintsUrl : Int -> String
tableConstraintsUrl =
    tableUrl >> flip (++) "/constraints"


indexForTable : Int -> Request TableConstraints
indexForTable tableId =
    Http.get
        (tableConstraintsUrl tableId)
        (dataDecoder Constraints.tableConstraintsDecoder)


destroy : Int -> Request ()
destroy id =
    delete (columnUrl id)
