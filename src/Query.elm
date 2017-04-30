module Query
    exposing
        ( Query
        , KnownValue
        , ResponseParseError
        , Model
        , Msg
        , queryInit
        , SubUpdate
        , queryUpdate
        , queryView
        , querySubscriptions
        , newQuery
        , program
        , programWithFlags
        , int
        , float
        , bool
        , string
        , unit
        , pair
        , oneOf
        , maybe
        , list
        , RecordQuery
        , record
        , field
        , known
        , fromRecordQuery
        , map
        , map2
        , map3
        , map4
        , map5
        , map6
        , map7
        , map8
        , succeed
        , andMap
        , equals
        , queryResponseToString
        , toRawQuery
        , parseModel
        , parseQuery
        , SelectResult
        , SelectError
        , select
        )

{-|
@docs Query, ResponseParseError

# Primitive Queries
@docs int, float, bool, string, unit

# Bigger Structures
@docs pair, oneOf, maybe, list

# Records
@docs RecordQuery, record, field, known, fromRecordQuery

# Known Values
@docs KnownValue, known

# Mapping
@docs map, map2, map3, map4, map5, map6, map7, map8

# Fancy Mapping
@docs succeed, andMap

# Comparison
@docs equals

# Raw Queries
@docs queryResponseToString, toRawQuery, parseModel, parseQuery

# Selection
@docs SelectResult, SelectError, select
-}

import Combine exposing (Parser, (*>), (<*))
import Combine.Num as Combine
import Dict exposing (Dict)
import Internal.Types exposing (..)
import Html exposing (Html)
import Result.Extra as Result
import WebSocket


-- Types


{-| TODO
-}
type alias Query a =
    Internal.Types.Query a


type alias QueryResponse =
    Internal.Types.QueryResponse


{-| TODO
-}
type alias KnownValue a =
    Internal.Types.KnownValue a


type alias QueryExpression =
    Internal.Types.QueryExpression


type alias QueryParseError =
    Internal.Types.QueryParseError


{-| TODO
-}
type alias ResponseParseError =
    Internal.Types.ResponseParseError



-- Model


type alias Model subModel =
    { subModel : subModel
    , query : Query subModel
    }


queryInit : String -> ( subModel, Query subModel, Cmd subMsg ) -> ( Model subModel, Cmd (Msg subModel subMsg) )
queryInit webSocketUrl ( initModel, initQuery, cmds ) =
    let
        mappedCmd =
            Cmd.map SubMsg cmds
    in
        ( Model initModel initQuery, Cmd.batch (newQuery webSocketUrl initQuery :: [ mappedCmd ]) )



-- Msg


type Msg subModel subMsg
    = SubMsg subMsg
    | NewModel subModel
    | NoOp



-- Update


type alias SubUpdate subModel subMsg =
    subMsg -> subModel -> ( subModel, Query subModel, Cmd subMsg )


queryUpdate :
    String
    -> SubUpdate subModel subMsg
    -> Msg subModel subMsg
    -> Model subModel
    -> ( Model subModel, Cmd (Msg subModel subMsg) )
queryUpdate wsurl subUpdate msg model =
    case msg of
        SubMsg subMsg ->
            let
                ( subModel, queryFromUpdate, subCmds ) =
                    subUpdate subMsg model.subModel

                mappedCmds =
                    [ Cmd.map SubMsg subCmds ]

                cmds =
                    if equals queryFromUpdate model.query then
                        mappedCmds
                    else
                        newQuery wsurl queryFromUpdate :: mappedCmds
            in
                { model
                    | subModel = subModel
                    , query = queryFromUpdate
                }
                    ! cmds

        NewModel newModel ->
            { model | subModel = newModel } ! []

        NoOp ->
            model ! []



-- View


queryView : (subModel -> Html subMsg) -> Model subModel -> Html (Msg subModel subMsg)
queryView subView model =
    subView model.subModel
        |> Html.map SubMsg



-- Subscriptions


querySubscriptions : String -> (subModel -> Sub subMsg) -> Model subModel -> Sub (Msg subModel subMsg)
querySubscriptions wsurl subSubscriptions model =
    Sub.batch
        [ parseModel model.query
            >> Result.map NewModel
            >> Result.withDefault NoOp
            |> WebSocket.listen wsurl
        , Sub.map SubMsg (subSubscriptions model.subModel)
        ]



-- Commands


newQuery : String -> Query subModel -> Cmd (Msg subModel subMsg)
newQuery wsurl query =
    WebSocket.send wsurl (toRawQuery query)



-- Program


program :
    { webSocketUrl : String
    , init : ( model, Query model, Cmd msg )
    , update : msg -> model -> ( model, Query model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }
    -> Program Never (Model model) (Msg model msg)
program { webSocketUrl, init, update, subscriptions, view } =
    let
        ( initModel, initQuery, initCmd ) =
            init

        mappedCmd =
            Cmd.map SubMsg initCmd
    in
        Html.program
            { init = queryInit webSocketUrl init
            , update = queryUpdate webSocketUrl update
            , subscriptions = querySubscriptions webSocketUrl subscriptions
            , view = queryView view
            }


programWithFlags :
    { webSocketUrl : String
    , init : flags -> ( model, Query model, Cmd msg )
    , update : msg -> model -> ( model, Query model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }
    -> Program flags (Model model) (Msg model msg)
programWithFlags { webSocketUrl, init, update, subscriptions, view } =
    Html.programWithFlags
        { init = init >> queryInit webSocketUrl
        , update = queryUpdate webSocketUrl update
        , subscriptions = querySubscriptions webSocketUrl subscriptions
        , view = queryView view
        }



-- Error Helpers


wrongType : QueryExpression -> QueryResponse -> Result ResponseParseError a
wrongType expected actual =
    Err (WrongType expected actual)


expectedKnown : QueryResponse -> QueryResponse -> Result ResponseParseError a
expectedKnown expected actual =
    Err (ExpectedKnown expected actual)



-- Primitive Queries


{-| TODO
-}
int : Query Int
int =
    { queryAST = QueryInt
    , parser =
        (\queryResponse ->
            case queryResponse of
                ResponseInt x ->
                    Ok x

                _ ->
                    wrongType QueryInt queryResponse
        )
    }


{-| TODO
-}
float : Query Float
float =
    { queryAST = QueryFloat
    , parser =
        (\queryResponse ->
            case queryResponse of
                ResponseFloat x ->
                    Ok x

                _ ->
                    wrongType QueryFloat queryResponse
        )
    }


{-| TODO
-}
bool : Query Bool
bool =
    { queryAST = QueryBool
    , parser =
        (\queryResponse ->
            case queryResponse of
                ResponseBool x ->
                    Ok x

                _ ->
                    wrongType QueryBool queryResponse
        )
    }


{-| TODO
-}
string : Query String
string =
    { queryAST = QueryString
    , parser =
        (\queryResponse ->
            case queryResponse of
                ResponseString x ->
                    Ok x

                _ ->
                    wrongType QueryString queryResponse
        )
    }


{-| TODO
-}
unit : Query ()
unit =
    { queryAST = QueryUnit
    , parser =
        (\queryResponse ->
            case queryResponse of
                ResponseUnit ->
                    Ok ()

                _ ->
                    wrongType QueryUnit queryResponse
        )
    }



-- Bigger Structures


{-| TODO
-}
pair : Query a -> Query b -> Query ( a, b )
pair query1 query2 =
    map2 (,) query1 query2


{-| TODO
-}
oneOf : List (Query a) -> Query a
oneOf queries =
    let
        expected =
            QuerySum (List.map .queryAST queries)

        findParsedResult queryResponse currentQuery previousResult =
            case previousResult of
                Ok _ ->
                    previousResult

                Err _ ->
                    currentQuery.parser queryResponse
    in
        { queryAST = expected
        , parser =
            (\queryResponse ->
                List.foldl (findParsedResult queryResponse) (Err BaseCaseForOneOf) queries
            )
        }


{-| TODO
-}
maybe : Query a -> Query (Maybe a)
maybe query =
    let
        justQuery =
            map Just query

        nothingQuery =
            map (always Nothing) unit
    in
        oneOf [ justQuery, nothingQuery ]


{-| TODO
-}
list : Query a -> Query (List a)
list query =
    let
        expected =
            QueryList query.queryAST

        buildResultList ( index, listItemParseResult ) resultList =
            case resultList of
                Ok list ->
                    case listItemParseResult of
                        Ok listItem ->
                            Ok (listItem :: list)

                        Err err ->
                            Err (ErrorAtIndex index err)

                _ ->
                    resultList
    in
        { queryAST = expected
        , parser =
            (\queryResponse ->
                case queryResponse of
                    ResponseList responseList ->
                        List.map query.parser responseList
                            |> List.indexedMap (,)
                            |> List.foldr buildResultList (Ok [])

                    _ ->
                        wrongType expected queryResponse
            )
        }


dict : Query comparable -> Query value -> Query (Dict comparable value)
dict keyQuery valueQuery =
    let
        expected =
            QueryDict keyQuery.queryAST valueQuery.queryAST

        buildResultDict ( keyResponse, valueResponse ) result =
            let
                keyResult =
                    keyQuery.parser keyResponse

                valueResult =
                    valueQuery.parser valueResponse
            in
                case result of
                    Ok dict ->
                        case keyResult of
                            Ok key ->
                                case valueResult of
                                    Ok value ->
                                        Ok (Dict.insert key value dict)

                                    Err err ->
                                        Err (ErrorAtKey keyResponse err)

                            Err err ->
                                Err (ErrorWithKey keyQuery.queryAST keyResponse)

                    _ ->
                        result
    in
        { queryAST = expected
        , parser =
            (\queryResponse ->
                case queryResponse of
                    ResponseDict responseKeyValuePairs ->
                        List.foldl buildResultDict (Ok Dict.empty) responseKeyValuePairs

                    _ ->
                        wrongType expected queryResponse
            )
        }



-- Records


{-| TODO
-}
type alias RecordQuery a =
    { fields : Dict String QueryExpression
    , parser : QueryResponse -> Result ResponseParseError a
    }


{-| TODO
-}
record : a -> RecordQuery a
record ctor =
    { fields = Dict.empty
    , parser = (\_ -> Ok ctor)
    }


{-| TODO
-}
field : String -> Query a -> RecordQuery (a -> b) -> RecordQuery b
field name query previous =
    { fields = Dict.insert name query.queryAST previous.fields
    , parser =
        (\queryResponse ->
            let
                previousFn =
                    previous.parser queryResponse
            in
                case queryResponse of
                    ResponseRecord record ->
                        Dict.get name record
                            |> Maybe.map query.parser
                            |> Maybe.map (flip Result.andMap previousFn)
                            |> Maybe.map (Result.mapError (ErrorAtKey (ResponseString name)))
                            |> Maybe.withDefault (Err (KeyNotFound name queryResponse))

                    _ ->
                        wrongType (QueryRecord (Dict.singleton name query.queryAST)) queryResponse
        )
    }


{-| TODO
-}
fromRecordQuery : RecordQuery a -> Query a
fromRecordQuery recordQuery =
    { queryAST = QueryRecord recordQuery.fields
    , parser = recordQuery.parser
    }



-- Known Values


{-| TODO
-}
known : KnownValue a -> Query a
known (KnownValue x expectedQueryResponse) =
    { queryAST = Known expectedQueryResponse
    , parser =
        (\queryResponse ->
            if queryResponse == expectedQueryResponse || expectedQueryResponse == ResponseIgnoreThis then
                Ok x
            else
                expectedKnown expectedQueryResponse queryResponse
        )
    }



-- Mapping


{-| TODO
-}
map : (a -> b) -> Query a -> Query b
map f query =
    { query | parser = query.parser >> Result.map f }


{-| TODO
-}
map2 :
    (a -> b -> c)
    -> Query a
    -> Query b
    -> Query c
map2 f query1 query2 =
    let
        expected =
            QueryProduct query1.queryAST query2.queryAST
    in
        { queryAST = expected
        , parser =
            (\queryResponse ->
                case ( query1.queryAST, query2.queryAST ) of
                    ( QueryIgnoreThis, _ ) ->
                        Result.map2 f (query1.parser ResponseUnit) (query2.parser queryResponse)

                    ( _, QueryIgnoreThis ) ->
                        Result.map2 f (query1.parser queryResponse) (query2.parser ResponseUnit)

                    _ ->
                        case queryResponse of
                            ResponseProduct subResponse1 subResponse2 ->
                                Result.map2 f (query1.parser subResponse1) (query2.parser subResponse2)

                            _ ->
                                wrongType expected queryResponse
            )
        }


{-| TODO
-}
map3 :
    (a -> b -> c -> d)
    -> Query a
    -> Query b
    -> Query c
    -> Query d
map3 f query1 query2 query3 =
    succeed f
        |> andMap query1
        |> andMap query2
        |> andMap query3


{-| TODO
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Query a
    -> Query b
    -> Query c
    -> Query d
    -> Query e
map4 f query1 query2 query3 query4 =
    succeed f
        |> andMap query1
        |> andMap query2
        |> andMap query3
        |> andMap query4


{-| TODO
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Query a
    -> Query b
    -> Query c
    -> Query d
    -> Query e
    -> Query f
map5 f query1 query2 query3 query4 query5 =
    succeed f
        |> andMap query1
        |> andMap query2
        |> andMap query3
        |> andMap query4
        |> andMap query5


{-| TODO
-}
map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Query a
    -> Query b
    -> Query c
    -> Query d
    -> Query e
    -> Query f
    -> Query g
map6 f query1 query2 query3 query4 query5 query6 =
    succeed f
        |> andMap query1
        |> andMap query2
        |> andMap query3
        |> andMap query4
        |> andMap query5
        |> andMap query6


{-| TODO
-}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> Query a
    -> Query b
    -> Query c
    -> Query d
    -> Query e
    -> Query f
    -> Query g
    -> Query h
map7 f query1 query2 query3 query4 query5 query6 query7 =
    succeed f
        |> andMap query1
        |> andMap query2
        |> andMap query3
        |> andMap query4
        |> andMap query5
        |> andMap query6
        |> andMap query7


{-| TODO
-}
map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i)
    -> Query a
    -> Query b
    -> Query c
    -> Query d
    -> Query e
    -> Query f
    -> Query g
    -> Query h
    -> Query i
map8 f query1 query2 query3 query4 query5 query6 query7 query8 =
    succeed f
        |> andMap query1
        |> andMap query2
        |> andMap query3
        |> andMap query4
        |> andMap query5
        |> andMap query6
        |> andMap query7
        |> andMap query8



-- Fancy Queries


{-| TODO
-}
succeed : a -> Query a
succeed x =
    { queryAST = QueryIgnoreThis
    , parser = (\_ -> Ok x)
    }


{-| TODO
-}
andMap : Query a -> Query (a -> b) -> Query b
andMap =
    flip (map2 (<|))



-- Comparison


{-| TODO
-}
equals : Query a -> Query a -> Bool
equals query1 query2 =
    query1.queryAST == query2.queryAST



-- Raw Queries


keyValuePairString : ( String, String ) -> String
keyValuePairString ( k, v ) =
    k ++ ":" ++ v


{-| TODO
-}
queryResponseToString : QueryResponse -> String
queryResponseToString queryResponse =
    case queryResponse of
        ResponseInt x ->
            toString x

        ResponseFloat x ->
            toString x

        ResponseBool x ->
            toString x

        ResponseString x ->
            toString x

        ResponseUnit ->
            "Unit"

        ResponseList list ->
            List.map queryResponseToString list
                |> List.intersperse ","
                |> List.foldl (++) ""

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


{-| TODO
-}
toRawQuery : Query a -> String
toRawQuery { queryAST, parser } =
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
    (Combine.string "\"" *> Combine.regex "(\\\\\"|[^\"\n])*" <* Combine.string "\"")


{-| TODO
-}
responseParser : Parser () QueryResponse
responseParser =
    let
        this =
            Combine.lazy (\_ -> responseParser)

        intParser =
            Combine.int |> Combine.map ResponseInt

        floatParser =
            Combine.float |> Combine.map ResponseFloat

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


{-| TODO
-}
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


{-| TODO
-}
parseModel : Query model -> String -> Result ResponseParseError model
parseModel query toParse =
    Combine.parse responseParser toParse
        |> Result.mapError (\( _, _, errorMessages ) -> ResponseFromStringError errorMessages)
        |> Result.andThen (\( _, _, response ) -> query.parser response)


{-| TODO
-}
parseQuery : String -> Result QueryParseError QueryExpression
parseQuery toParse =
    Combine.parse astParser toParse
        |> Result.mapError (\( _, _, errorMessages ) -> QueryFromStringError errorMessages)
        |> Result.map (\( _, _, queryAST ) -> queryAST)



-- Selection


{-| TODO
-}
type alias SelectError =
    Internal.Types.SelectError


{-| TODO
-}
type alias SelectResult =
    Result SelectError QueryResponse


type alias RecordSelectResult =
    Result SelectError (Dict String QueryResponse)


{-| TODO
-}
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
