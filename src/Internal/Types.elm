module Internal.Types exposing (..)

import Dict exposing (Dict)


type QueryResponse
    = ResponseInt Int
    | ResponseFloat Float
    | ResponseBool Bool
    | ResponseString String
    | ResponseUnit
    | ResponseList (List QueryResponse)
    | ResponseProduct QueryResponse QueryResponse
    | ResponseDict (List ( QueryResponse, QueryResponse ))
    | ResponseRecord (Dict String QueryResponse)
    | ResponseIgnoreThis


type KnownValue a
    = KnownValue a QueryResponse


type QueryExpression
    = QueryInt
    | QueryFloat
    | QueryBool
    | QueryString
    | QueryUnit
    | Known QueryResponse
    | QuerySum (List QueryExpression)
    | QueryProduct QueryExpression QueryExpression
    | QueryList QueryExpression
    | QueryDict QueryExpression QueryExpression
    | QueryRecord (Dict String QueryExpression)
    | QueryIgnoreThis


type alias Query a =
    { queryAST : QueryExpression
    , parser : QueryResponse -> Result ResponseParseError a
    }


type QueryParseError
    = QueryFromStringError {- TODO: turn these into more errors -} (List String)


type SelectError
    = MatchFailure
    | UnmatchedKnown
    | SelectErrorAtIndex Int SelectError
    | SelectProductLeftError SelectError
    | SelectProductRightError SelectError
    | UnknownField String
    | AggregateSelectErrors (List SelectError)


type ResponseParseError
    = WrongType QueryExpression {- expected -} QueryResponse {- actual -}
    | ExpectedKnown QueryResponse {- expected -} QueryResponse {- actual -}
    | BaseCaseForOneOf
    | ErrorAtIndex Int ResponseParseError
    | ErrorAtKey QueryResponse ResponseParseError
    | ErrorWithKey QueryExpression {- expected key type -} QueryResponse {- actual key type -}
    | KeyNotFound String QueryResponse
    | ResponseFromStringError {- TODO: turn these into more errors -} (List String)
    | MoreFieldsThanRequested
