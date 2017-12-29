module Request.Constraint
    exposing
        ( destroy
        , indexForTable
        )

import Data.Constraint as Constraint exposing (Constraint)
import Http exposing (Request)
import Json.Decode as JD
import Request.Table as TableReq
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


constraintsUrl : String
constraintsUrl =
    baseUrl ++ "constraints/"


columnUrl : Int -> String
columnUrl =
    toString >> (++) constraintsUrl


tableConstraintsUrl : Int -> String
tableConstraintsUrl =
    TableReq.resourceUrl >> flip (++) "/constraints"


indexForTable : Int -> Request (List Constraint)
indexForTable tableId =
    Http.get
        (tableConstraintsUrl tableId)
        (dataDecoder (JD.list Constraint.decoder))


destroy : Int -> Request ()
destroy id =
    delete (columnUrl id)
