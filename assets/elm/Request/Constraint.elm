module Request.Constraint
    exposing
        ( destroy
        , indexForTable
          --, one
          --, update
        )

import Data.Constraints as Constraints exposing (Constraints)
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



--one : Int -> Request Constraints
--one id =
--    Http.get (columnUrl id) (dataDecoder Constraints.decoder)


indexForTable : Int -> Request Constraints
indexForTable tableId =
    Http.get
        (tableConstraintsUrl tableId)
        (dataDecoder Constraints.decoder)



--update : Constraints -> Request Constraints
--update column =
--    put (columnUrl column.id)
--        (Constraints.encode column |> Http.jsonBody)
--        (dataDecoder Constraints.decoder)


destroy : Int -> Request ()
destroy id =
    delete (columnUrl id)
