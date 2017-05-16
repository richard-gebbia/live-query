module Query.Advanced exposing (..)

import Combine exposing (Parser, (*>), (<*))
import Combine.Num as Combine
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


type Query a
    = Query
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



-- Raw Queries


keyValuePairString : ( String, String ) -> String
keyValuePairString ( k, v ) =
    k ++ ":" ++ v


{-| Turns a `QueryResponse` into a raw string to be sent
over a network and parsed on the other side. Primarily used
by servers.
-}
queryResponseToString : QueryResponse -> String
queryResponseToString queryResponse =
    case queryResponse of
        ResponseInt x ->
            toString x

        ResponseFloat x ->
            toString x ++ "f"

        ResponseBool x ->
            toString x

        ResponseString x ->
            toString x

        ResponseUnit ->
            "Unit"

        ResponseList list ->
            List.map queryResponseToString list
                |> List.intersperse ","
                |> List.foldr (++) ""
                |> (\s -> "[" ++ s ++ "]")

        ResponseDict entries ->
            let
                onPair : (a -> b) -> ( a, a ) -> ( b, b )
                onPair f ( x, y ) =
                    ( f x, f y )
            in
                "{"
                    ++ (List.map (onPair queryResponseToString) entries
                            |> List.map keyValuePairString
                            |> List.intersperse ","
                            |> List.foldl (++) ""
                       )
                    ++ "}"

        ResponseRecord record ->
            "{"
                ++ (Dict.toList record
                        |> List.map (\( name, value ) -> ( "\"" ++ name ++ "\"", queryResponseToString value ))
                        |> List.map keyValuePairString
                        |> List.intersperse ";"
                        |> List.foldl (++) ""
                   )
                ++ "}"

        ResponseProduct subResponse1 subResponse2 ->
            if subResponse1 == ResponseIgnoreThis then
                queryResponseToString subResponse2
            else if subResponse2 == ResponseIgnoreThis then
                queryResponseToString subResponse1
            else
                "(" ++ queryResponseToString subResponse1 ++ "," ++ queryResponseToString subResponse2 ++ ")"

        ResponseIgnoreThis ->
            ""


{-| Turns a `Query` into a raw string to be sent over a
network and parsed on the other side. Primary used by clients.
-}
toRawQuery : Query a -> String
toRawQuery (Query { queryAST, parser }) =
    toRawQueryFromAST queryAST


toRawQueryFromAST : QueryExpression -> String
toRawQueryFromAST queryAST =
    case queryAST of
        QueryInt ->
            "Int"

        QueryFloat ->
            "Float"

        QueryBool ->
            "Bool"

        QueryString ->
            "String"

        QueryUnit ->
            "Unit"

        Known queryResponse ->
            queryResponseToString queryResponse

        QuerySum candidates ->
            List.map toRawQueryFromAST candidates
                |> List.intersperse "|"
                |> List.foldl (++) "|"

        QueryProduct subQuery1 subQuery2 ->
            if subQuery1 == QueryIgnoreThis then
                toRawQueryFromAST subQuery2
            else if subQuery2 == QueryIgnoreThis then
                toRawQueryFromAST subQuery1
            else
                "(" ++ toRawQueryFromAST subQuery1 ++ "," ++ toRawQueryFromAST subQuery2 ++ ")"

        QueryList listQuery ->
            "[" ++ toRawQueryFromAST listQuery ++ "]"

        QueryDict keyQuery valueQuery ->
            "{" ++ toRawQueryFromAST keyQuery ++ ":" ++ toRawQueryFromAST valueQuery ++ "}"

        QueryRecord record ->
            "{"
                ++ (Dict.toList record
                        |> List.map (\( name, value ) -> ( "\"" ++ name ++ "\"", toRawQueryFromAST value ))
                        |> List.map keyValuePairString
                        |> List.intersperse ";"
                        |> List.foldl (++) ""
                   )
                ++ "}"

        QueryIgnoreThis ->
            ""



-- TODO: errors


skipThen : Parser s b -> Parser s a -> Parser s a
skipThen parserToSkip nextParser =
    (Combine.skip parserToSkip
        |> Combine.andThen (\_ -> nextParser)
    )


rawStringParser : Parser s String
rawStringParser =
    -- thanks Bogdanp
    (Combine.string "\"" *> Combine.regex "(\\\\\"|\\\\\\\\|[^\"\n])*" <* Combine.string "\"")


responseParser : Parser () QueryResponse
responseParser =
    let
        this =
            Combine.lazy (\_ -> responseParser)

        intParser =
            Combine.int |> Combine.map ResponseInt

        floatParser =
            Combine.choice
                [ Combine.float <* Combine.string "f"
                , Combine.int <* Combine.string "f" |> Combine.map toFloat
                ]
                |> Combine.map ResponseFloat

        boolParser =
            Combine.or
                (Combine.string "True" |> Combine.map (\_ -> ResponseBool True))
                (Combine.string "False" |> Combine.map (\_ -> ResponseBool False))

        stringParser =
            rawStringParser
                |> Combine.map ResponseString

        unitParser =
            Combine.string "Unit" |> Combine.map (\_ -> ResponseUnit)

        listParser =
            Combine.brackets (Combine.sepBy (Combine.string ",") this)
                |> Combine.map ResponseList

        dictParser =
            let
                keyValueParser =
                    this
                        |> Combine.map (,)
                        |> Combine.andMap (skipThen (Combine.string ":") this)
            in
                Combine.braces (Combine.sepBy (Combine.string ",") keyValueParser)
                    |> Combine.map ResponseDict

        recordParser =
            let
                keyValueParser =
                    rawStringParser
                        |> Combine.map (,)
                        |> Combine.andMap (skipThen (Combine.string ":") this)
            in
                Combine.braces (Combine.sepBy (Combine.string ";") keyValueParser)
                    |> Combine.map Dict.fromList
                    |> Combine.map ResponseRecord

        productParser =
            Combine.parens
                (this
                    |> Combine.map ResponseProduct
                    |> Combine.andMap (skipThen (Combine.string ",") this)
                )
    in
        Combine.choice
            [ floatParser
            , intParser
            , boolParser
            , stringParser
            , unitParser
            , listParser
            , dictParser
            , recordParser
            , productParser
            ]


astParser : Parser () QueryExpression
astParser =
    let
        this =
            Combine.lazy (\_ -> astParser)

        primitiveParser =
            Combine.choice
                [ Combine.string "Int" |> Combine.map (\_ -> QueryInt)
                , Combine.string "Float" |> Combine.map (\_ -> QueryFloat)
                , Combine.string "Bool" |> Combine.map (\_ -> QueryBool)
                , Combine.string "String" |> Combine.map (\_ -> QueryString)
                , Combine.string "Unit" |> Combine.map (\_ -> QueryUnit)
                ]

        productParser =
            Combine.parens
                (this
                    |> Combine.map QueryProduct
                    |> Combine.andMap (skipThen (Combine.string ",") this)
                )

        sumParser =
            Combine.many1 (skipThen (Combine.string "|") this)
                |> Combine.map QuerySum

        listParser =
            Combine.brackets this
                |> Combine.map QueryList

        dictParser =
            Combine.braces
                (this
                    |> Combine.map QueryDict
                    |> Combine.andMap (skipThen (Combine.string ":") this)
                )

        recordParser =
            let
                keyValueParser =
                    rawStringParser
                        |> Combine.map (,)
                        |> Combine.andMap (skipThen (Combine.string ":") this)
            in
                Combine.braces (Combine.sepBy (Combine.string ";") keyValueParser)
                    |> Combine.map Dict.fromList
                    |> Combine.map QueryRecord

        knownValueParser =
            responseParser
                |> Combine.map Known
    in
        Combine.choice
            [ primitiveParser
            , productParser
            , sumParser
            , listParser
            , dictParser
            , recordParser
            , knownValueParser
            ]


{-| Given a query for a `model` and a string response from the server,
try to parse that string into an instance of a `model`.
-}
parseModel : Query model -> String -> Result ResponseParseError model
parseModel (Query query) toParse =
    Combine.parse responseParser toParse
        |> Result.mapError (\( _, _, errorMessages ) -> ResponseFromStringError errorMessages)
        |> Result.andThen (\( _, _, response ) -> query.parser response)


{-| Try to parse a string that was generated from `toRawQuery`.
Primarily used on the server.
-}
parseQuery : String -> Result QueryParseError QueryExpression
parseQuery toParse =
    Combine.parse astParser toParse
        |> Result.mapError (\( _, _, errorMessages ) -> QueryFromStringError errorMessages)
        |> Result.map (\( _, _, queryAST ) -> queryAST)



-- Selection


type alias SelectResult =
    Result SelectError QueryResponse


type alias RecordSelectResult =
    Result SelectError (Dict String QueryResponse)


select : QueryExpression -> QueryResponse -> SelectResult
select query response =
    case ( query, response ) of
        ( QueryInt, ResponseInt x ) ->
            Ok (ResponseInt x)

        ( QueryFloat, ResponseFloat x ) ->
            Ok (ResponseFloat x)

        ( QueryBool, ResponseBool x ) ->
            Ok (ResponseBool x)

        ( QueryString, ResponseString x ) ->
            Ok (ResponseString x)

        ( QueryUnit, ResponseUnit ) ->
            Ok ResponseUnit

        ( Known knownResponse, response ) ->
            if response == knownResponse then
                Ok response
            else
                Err UnmatchedKnown

        ( QuerySum possibleMatches, response ) ->
            let
                findAMatch possibleMatch result =
                    case result of
                        Ok matchedResponse ->
                            Ok matchedResponse

                        _ ->
                            select possibleMatch response
            in
                List.foldr findAMatch (Err MatchFailure) possibleMatches

        ( QueryList listTypeQuery, ResponseList list ) ->
            let
                hasError ( index, result ) =
                    case result of
                        Ok _ ->
                            Nothing

                        Err msg ->
                            Just ( index, msg )

                aggregateErrors errors =
                    case errors of
                        [] ->
                            Ok (ResponseList list)

                        _ ->
                            Err (AggregateSelectErrors errors)
            in
                List.map (select listTypeQuery) list
                    |> List.indexedMap (,)
                    |> List.filterMap hasError
                    |> List.map (\( index, error ) -> SelectErrorAtIndex index error)
                    |> aggregateErrors

        ( QueryProduct subQuery1 subQuery2, ResponseProduct response1 response2 ) ->
            let
                result1 =
                    select subQuery1 response1

                result2 =
                    select subQuery2 response2
            in
                case ( result1, result2 ) of
                    ( Ok response1, Ok response2 ) ->
                        Ok (ResponseProduct response1 response2)

                    ( Err msg, Ok _ ) ->
                        Err (SelectProductLeftError msg)

                    ( Ok _, Err msg ) ->
                        Err (SelectProductRightError msg)

                    ( Err msg1, Err msg2 ) ->
                        Err (AggregateSelectErrors [ SelectProductLeftError msg1, SelectProductRightError msg2 ])

        ( QueryRecord queryFields, ResponseRecord responseFields ) ->
            selectFromRecord queryFields responseFields
                |> Result.map ResponseRecord

        _ ->
            Err MatchFailure


selectFromRecord : Dict String QueryExpression -> Dict String QueryResponse -> RecordSelectResult
selectFromRecord queryFields responseFields =
    let
        makeError : RecordSelectResult -> SelectError -> RecordSelectResult
        makeError currentResult currentError =
            case currentResult of
                Err (AggregateSelectErrors errors) ->
                    Err (AggregateSelectErrors (currentError :: errors))

                Err msg ->
                    Err (AggregateSelectErrors [ currentError, msg ])

                _ ->
                    Err currentError

        inQueryButNotInResponse : String -> QueryExpression -> RecordSelectResult -> RecordSelectResult
        inQueryButNotInResponse fieldName query currentResult =
            makeError currentResult (UnknownField fieldName)

        inBothQueryAndResponse : String -> QueryExpression -> QueryResponse -> RecordSelectResult -> RecordSelectResult
        inBothQueryAndResponse fieldName query response currentResult =
            let
                selectFieldResult =
                    select query response
            in
                case selectFieldResult of
                    Ok response ->
                        Result.map (Dict.insert fieldName response) currentResult

                    Err msg ->
                        makeError currentResult msg

        inResponseButNotInQuery : String -> QueryResponse -> RecordSelectResult -> RecordSelectResult
        inResponseButNotInQuery _ _ currentResult =
            currentResult
    in
        Dict.merge
            inQueryButNotInResponse
            inBothQueryAndResponse
            inResponseButNotInQuery
            queryFields
            responseFields
            (Ok Dict.empty)
