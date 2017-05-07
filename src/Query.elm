module Query
    exposing
        ( Query
        , KnownValue
        , ResponseParseError
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
        , fromRecordQuery
        , known
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
        )

{-| Live queries let you represent your client model or your view as a function of your server model.
You can then set up your front-end to automatically update when the server does without having to
write a bunch of code to send requests to the server or parse its responses.

# Primary types
@docs Query, ResponseParseError

# Query programs
@docs program, programWithFlags

# Primitive Queries
@docs int, float, bool, string, unit

# Records
@docs RecordQuery, record, field, fromRecordQuery

# Known Values
@docs KnownValue, known

# Bigger Structures
@docs pair, oneOf, maybe, list

# Mapping
@docs map, map2, map3, map4, map5, map6, map7, map8

# Fancy Mapping
@docs succeed, andMap

# Comparison
@docs equals
-}

import Dict exposing (Dict)
import Html exposing (Html)
import Result.Extra as Result
import Query.Advanced exposing (..)
import Query.Known as Known
import WebSocket


-- Types


{-| `Query a` can be read as "a live query for `a`". Once you've built a `Query`,
you can subscribe to it on the server, and, if all goes well, every time that value
changes on the server, you'll get notified.

For instance, subscribing to a `Query String` implies that the server has an `String`
on it somewhere that you care about. Every time that `String` changes on the server, your
`Query String` will get a copy of its new value on the client. This allows you write
extremely declarative and concise code:
```
stringOnServer = Query.string

view = Query.map Html.text stringOnServer
```
Also, and probably more importantly, you never had to build a Json decoder or a
Http request to get it, which can be tricky and error-prone.
-}
type alias Query a =
    Query.Advanced.Query a


type alias QueryResponse =
    Query.Advanced.QueryResponse


{-| A `KnownValue` is, as its name implies, a value that you already know about on
the client but is still part of the query. A known value is used to *narrow down*
the query to a more specific piece of data from the server.

For example, let's say your app has a concept of users. A user looks like this:
```
type alias User =
    { userId: Int
    , name: String
    , ...
    }
```

You can think of at least some part of your server model as being a `List User`.
On your client, though, you only ever care about one user at a time, so you'd
rather just fetch data for that user instead of all the users. Let's say you know
that the user's `userId` is 3. In that case, you can use a `KnownValue` for the
`userId` like this:
```
userData : Query User
userData =
    record User
        |> field "userId" (known (Known.int 3))
        |> field "name" string
        |> ...
        |> fromRecordQuery
```
-}
type alias KnownValue a =
    Known.KnownValue a


{-| The primary data structure that represents a query.
You probably won't be dealing with this unless you're trying to
optimize server-side computations.

If you find yourself needing to look at the innards of a query,
check out the `Advanced` module, which exposes all the constructors
of this type.
-}
type alias QueryExpression =
    Query.Advanced.QueryExpression


type alias QueryParseError =
    Query.Advanced.QueryParseError


{-| An error that is generated when an update from the server arrives that
can't be parsed.
-}
type alias ResponseParseError =
    Query.Advanced.ResponseParseError



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


{-| Create a [`Program`][program] that describes how your app works.
Note that your `init` and `update` functions should produce a triple
with the `Query model` as the second element. This program will set up
a websocket connection with a compatible server for live updates.
Since the `Query` is for your `model`, that `model` will be updated every
time the server pushes a new update.

Read about [The Elm Architecture][tea] to learn how to use this.

[program]: http://package.elm-lang.org/packages/elm-lang/core/latest/Platform#Program
[tea]: https://guide.elm-lang.org/architecture/
-}
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


{-| Create a [`Program`][program] that describes how your app works.
Note that your `init` and `update` functions should produce a triple
with the `Query model` as the second element. This program will set up
a websocket connection with a compatible server for live updates.
Since the `Query` is for your `model`, that `model` will be updated every
time the server pushes a new update.

Read about [The Elm Architecture][tea] to learn how to use this.

[program]: http://package.elm-lang.org/packages/elm-lang/core/latest/Platform#Program
[tea]: https://guide.elm-lang.org/architecture/
-}
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


{-| Query for an `Int` in the server's data model. When that value changes
on the server, you'll be notified on the client.
-}
int : Query Int
int =
    Query
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


{-| Query for a `Float` in the server's data model. When that value changes
on the server, you'll be notified on the client.
-}
float : Query Float
float =
    Query
        { queryAST = QueryFloat
        , parser =
            (\queryResponse ->
                case queryResponse of
                    ResponseInt x ->
                        Ok (toFloat x)

                    ResponseFloat x ->
                        Ok x

                    _ ->
                        wrongType QueryFloat queryResponse
            )
        }


{-| Query for a `Bool` in the server's data model. When that value changes
on the server, you'll be notified on the client.
-}
bool : Query Bool
bool =
    Query
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


{-| Query for a `String` in the server's data model. When that value changes
on the server, you'll be notified on the client.
-}
string : Query String
string =
    Query
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


{-| Query for a `()` in the server's data model. That value will never
change, so you'll likely be using this in a similar fashion to a regular
`()` -- ignoring values you don't care about.
-}
unit : Query ()
unit =
    Query
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


{-| Query for a pair of values in the server's data model.

```
myPairQuery : Query (Int, String)
myPairQuery =
    pair int string
```
-}
pair : Query a -> Query b -> Query ( a, b )
pair query1 query2 =
    map2 (,) query1 query2


{-| Try a bunch of different queries in order.
```
trueOrFalseQuery =
    oneOf
        [ known (Known.string "True")
        , known (Known.string "False")
        ]
```
-}
oneOf : List (Query a) -> Query a
oneOf queries =
    let
        expected =
            QuerySum (List.map (\(Query { queryAST, parser }) -> queryAST) queries)

        findParsedResult queryResponse (Query { queryAST, parser }) previousResult =
            case previousResult of
                Ok _ ->
                    previousResult

                Err _ ->
                    parser queryResponse
    in
        Query
            { queryAST = expected
            , parser =
                (\queryResponse ->
                    List.foldl (findParsedResult queryResponse) (Err BaseCaseForOneOf) queries
                )
            }


{-| Query for a `Maybe` value on the server. In the `Just` case, it
is expected for the value to simply exist, and in the `Nothing` case the
value is expected to come back as `()`.

```
maybeIntQuery : Query (Maybe Int)
maybeIntQuery =
    maybe int

-- Server sends back: 3
-- Query will have value: Just 3

-- Server sends back: ()
-- Query will have value: Nothing
```
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


{-| Query for a list of values on the server.
```
listOfInts : Query (List Int)
listOfInts =
    list int
```
-}
list : Query a -> Query (List a)
list (Query { queryAST, parser }) =
    let
        expected =
            QueryList queryAST

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
        Query
            { queryAST = expected
            , parser =
                (\queryResponse ->
                    case queryResponse of
                        ResponseList responseList ->
                            List.map parser responseList
                                |> List.indexedMap (,)
                                |> List.foldr buildResultList (Ok [])

                        _ ->
                            wrongType expected queryResponse
                )
            }


{-| Query for a map of keys to values on the server.

namesToAges : Query (Dict String Int)
namesToAges =
    dict string int
-}
dict : Query comparable -> Query value -> Query (Dict comparable value)
dict (Query keyQuery) (Query valueQuery) =
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
        Query
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



-- Known Values


{-| Query for a `KnownValue`.
-}
known : Known.KnownValue a -> Query a
known =
    Known.query



-- Records


{-| A `RecordQuery` is, as its name implies, a live query for a record value
on the server. It is unique in that the set of fields queried is not required
to exactly match the same object on the server. It can be a subset of the fields,
in which case, only those fields will be returned from the server.

For example:
```
type alias ServerDataModel =
    { tweet: String
    , likes: Int
    , retweets: Int
    , author: String
    }

-- let's not show all the tweet information
type alias ClientDataModel =
    { tweet: String
    , author: String
    }

tweetAndAuthorQuery : RecordQuery ClientDataModel
tweetAndAuthorQuery =
    record ClientDataModel
        |> field "tweet" string
        |> field "author" string
```
-}
type RecordQuery a
    = RecordQuery
        { fields : Dict String QueryExpression
        , parser : QueryResponse -> Result ResponseParseError a
        }


{-| Creates a new `RecordQuery` given a constructor for a record type.
```
emptyRecordQuery : RecordQuery {}
emptyRecordQuery =
    record {}
```
-}
record : a -> RecordQuery a
record ctor =
    RecordQuery
        { fields = Dict.empty
        , parser = (\_ -> Ok ctor)
        }


{-| Adds a field to the supplied `RecordQuery`. The type signature
is designed similarly to `andMap`, so that you can make type-safe
record queries (you still need to get the field name right, though).

```
type alias User =
    { name: String
    , points: Int
    }

userQuery : RecordQuery User
userQuery =
    record User
        |> field "name" string
        |> field "points" int

-- this won't compile, since "points"
-- isn't a String field
userQuery_ : RecordQuery User
userQuery_ =
    record User
        |> field "name" string
        |> field "points" string
```
-}
field : String -> Query a -> RecordQuery (a -> b) -> RecordQuery b
field name (Query current) (RecordQuery previous) =
    RecordQuery
        { fields = Dict.insert name current.queryAST previous.fields
        , parser =
            (\queryResponse ->
                let
                    previousFn =
                        previous.parser queryResponse
                in
                    case queryResponse of
                        ResponseRecord record ->
                            Dict.get name record
                                |> Maybe.map current.parser
                                |> Maybe.map (flip Result.andMap previousFn)
                                |> Maybe.map (Result.mapError (ErrorAtKey (ResponseString name)))
                                |> Maybe.withDefault (Err (KeyNotFound name queryResponse))

                        _ ->
                            wrongType (QueryRecord (Dict.singleton name current.queryAST)) queryResponse
            )
        }


{-| Converts a `RecordQuery` to a regular `Query`.
-}
fromRecordQuery : RecordQuery a -> Query a
fromRecordQuery (RecordQuery { fields, parser }) =
    Query
        { queryAST = QueryRecord fields
        , parser = parser
        }



-- Mapping


{-| Transform a query. This is useful primarily for "casting" primitive values
into user-defined types.

```
type Age = Age Int

ageQuery : Query Age
ageQuery =
    map Age int
```

It is especially useful with `oneOf` to query for union types on the server.

```
resultQuery : Query (Result String Int)
resultQuery =
    oneOf
        [ map Ok int
        , map Err string
        ]
```
-}
map : (a -> b) -> Query a -> Query b
map f (Query query) =
    Query { query | parser = query.parser >> Result.map f }


{-| Query for two things at once and combine the result. We can use this
to query for objects with multiple fields.
```
type Point2D = Point2D Float Float

pointQuery : Query Point2D
pointQuery =
    map2 Point2D float float
```
-}
map2 :
    (a -> b -> c)
    -> Query a
    -> Query b
    -> Query c
map2 f (Query query1) (Query query2) =
    let
        expected =
            QueryProduct query1.queryAST query2.queryAST
    in
        Query
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


{-| Query for three things at once and combine the result. We can use this
to query for objects with multiple fields.
```
type Point3D = Point3D Float Float Float

pointQuery : Query Point3D
pointQuery =
    map3 Point3D float float float
```
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


{-| -}
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


{-| -}
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


{-| -}
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


{-| -}
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


{-| -}
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


{-| Ignore the server response and always return a particular value.
This is different from a `KnownValue` because a `KnownValue` is actually
sent to the server and used to filter results.

This is useful when combined with `oneOf` to create a "default case" for
when all other queries fail.

```
ageQuery : Query Int
ageQuery =
    oneOf
        [ int
        , succeed 42
        ]
```
-}
succeed : a -> Query a
succeed x =
    Query
        { queryAST = QueryIgnoreThis
        , parser = (\_ -> Ok x)
        }


{-| Create a query that depends on the result of another. Like `map2`, `map3`,
and the other map functions, this can be used to query for larger objects.

For instance, this query:
```
type Point3D = Point3D Float Float Float

pointQuery =
    map3 Point3D float float float
```

is equivalent to this one:
```
pointQuery =
    succeed Point3D
        |> andMap float
        |> andMap float
        |> andMap float
```
-}
andMap : Query a -> Query (a -> b) -> Query b
andMap =
    flip (map2 (<|))



-- Comparison


{-| Given two queries, tells whether or not they
query for the same value.
```
equals int int == True
equals string float == False
```
-}
equals : Query a -> Query a -> Bool
equals (Query query1) (Query query2) =
    query1.queryAST == query2.queryAST
