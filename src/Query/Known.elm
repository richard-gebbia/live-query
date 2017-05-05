module Query.Known
    exposing
        ( KnownValue
        , int
        , float
        , bool
        , string
        , unit
        , list
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
        , query
        , queryResponse
        , value
        )

{-|
@docs KnownValue, value

# Primitive Values
@docs int, float, bool, string, unit, list

# Mapping
@docs map, map2, map3, map4, map5, map6, map7, map8

# Fancy Mapping
@docs succeed, andMap

# Queries
@docs query, queryResponse
-}

import Query.Advanced as Types exposing (Query(..), QueryResponse)


{-| A `KnownValue` is, as its name implies, a value that you already know about on
the client but is still part of the query. A known value is used to *narrow down*
the query to a more specific piece of data from the server.

As an example, let's say you're building a program that solves algebraic
equations. The user inputs the equation they want solved on the client and the
server simulates the graph of the equation by plugging in points. The server
model, then, looks like this:

```
type alias ServerModel = List (Float, Float)
```

On the client, you only want to display *solutions* to that equation
(that is, points on the curve where x is 0). You can create that
query like so:

```
solutionsQuery : Query (Float, Float)
solutionsQuery =
    pair
        (known (Known.float 0.0))
        float
```
-}
type KnownValue a
    = KnownValue a QueryResponse


primitive : (a -> QueryResponse) -> a -> KnownValue a
primitive queryResponseConstructor x =
    KnownValue x (queryResponseConstructor x)


{-| Narrow down a query by indicating that you only want data where a piece
has a particular integer value.
-}
int : Int -> KnownValue Int
int =
    primitive Types.ResponseInt


{-| Narrow down a query by indicating that you only want data where a piece
has a particular floating-point value.
-}
float : Float -> KnownValue Float
float =
    primitive Types.ResponseFloat


{-| Narrow down a query by indicating that you only want data where a piece
has a particular boolean value.
-}
bool : Bool -> KnownValue Bool
bool =
    primitive Types.ResponseBool


{-| Narrow down a query by indicating that you only want data where a piece
has a particular string value.
-}
string : String -> KnownValue String
string =
    primitive Types.ResponseString


{-| Narrow down a query by indicating that you only want data where a piece
is a `()`. This is primarily used to query for `Maybe` values on the server
that you want to be `Nothing`.
-}
unit : KnownValue ()
unit =
    KnownValue () (Types.ResponseUnit)


{-| Narrow down a query by indicating that you only want data where a piece
is a particular list of values.
-}
list : List (KnownValue a) -> KnownValue (List a)
list knownValues =
    let
        value (KnownValue x _) =
            x

        queryResponse (KnownValue _ x) =
            x
    in
        KnownValue (List.map value knownValues) (Types.ResponseList (List.map queryResponse knownValues))


{-| Transform a known value. This is primarily used to "cast" primitive values into
user-defined types.
```
type UniqueId = UniqueId Int

knownUid : KnownValue UniqueId
knownUid =
    Known.map UniqueId (Known.int 3)
```

Note that this does not affect the *encoded* value, the value that would be
sent to the server or the client over the wire. For instance,

```
myKnownInt : KnownValue Int
myKnownInt =
    Known.map ((+) 2) (Known.int 6)
```

will be *encoded* as 6, not 8.
-}
map : (a -> b) -> KnownValue a -> KnownValue b
map f (KnownValue x queryResponse) =
    KnownValue (f x) queryResponse


{-| Combine two known values using a function that takes two arguments. This, along with
`andMap` are our primary tools for building bigger `KnownValue`s than just primitives.

```
type PeanutButter = Creamy | Crunchy
type Jelly = Grape | Apple
type PBnJ = PBnJ PeanutButter Jelly

knownPBnJ : KnownValue PBnJ
knownPBnJ =
    Known.map2 PBnJ
        (Known.string "Creamy"
            |> Known.map (\_ -> Creamy))
        (Known.string "Apple"
            |> Known.map (\_ -> Apple))
```
-}
map2 :
    (a -> b -> c)
    -> KnownValue a
    -> KnownValue b
    -> KnownValue c
map2 f (KnownValue value1 queryResponse1) (KnownValue value2 queryResponse2) =
    KnownValue (f value1 value2) (Types.ResponseProduct queryResponse1 queryResponse2)


{-| Combine three known values using a function that takes three arugments.
```
type NonEmptyList a = NonEmptyList (List a) a (List a)

knownNonEmptyIntList = Known (NonEmptyList Int)
knownNonEmptyIntList =
    Known.map3 NonEmptyList
        (Known.list [])
        (Known.int 4)
        (Known.list [Known.int 2, Known.int -3])
```
-}
map3 :
    (a -> b -> c -> d)
    -> KnownValue a
    -> KnownValue b
    -> KnownValue c
    -> KnownValue d
map3 f knownValue1 knownValue2 knownValue3 =
    succeed f
        |> andMap knownValue1
        |> andMap knownValue2
        |> andMap knownValue3


{-| -}
map4 :
    (a -> b -> c -> d -> e)
    -> KnownValue a
    -> KnownValue b
    -> KnownValue c
    -> KnownValue d
    -> KnownValue e
map4 f knownValue1 knownValue2 knownValue3 knownValue4 =
    succeed f
        |> andMap knownValue1
        |> andMap knownValue2
        |> andMap knownValue3
        |> andMap knownValue4


{-| -}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> KnownValue a
    -> KnownValue b
    -> KnownValue c
    -> KnownValue d
    -> KnownValue e
    -> KnownValue f
map5 f knownValue1 knownValue2 knownValue3 knownValue4 knownValue5 =
    succeed f
        |> andMap knownValue1
        |> andMap knownValue2
        |> andMap knownValue3
        |> andMap knownValue4
        |> andMap knownValue5


{-| -}
map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> KnownValue a
    -> KnownValue b
    -> KnownValue c
    -> KnownValue d
    -> KnownValue e
    -> KnownValue f
    -> KnownValue g
map6 f knownValue1 knownValue2 knownValue3 knownValue4 knownValue5 knownValue6 =
    succeed f
        |> andMap knownValue1
        |> andMap knownValue2
        |> andMap knownValue3
        |> andMap knownValue4
        |> andMap knownValue5
        |> andMap knownValue6


{-| -}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> KnownValue a
    -> KnownValue b
    -> KnownValue c
    -> KnownValue d
    -> KnownValue e
    -> KnownValue f
    -> KnownValue g
    -> KnownValue h
map7 f knownValue1 knownValue2 knownValue3 knownValue4 knownValue5 knownValue6 knownValue7 =
    succeed f
        |> andMap knownValue1
        |> andMap knownValue2
        |> andMap knownValue3
        |> andMap knownValue4
        |> andMap knownValue5
        |> andMap knownValue6
        |> andMap knownValue7


{-| -}
map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i)
    -> KnownValue a
    -> KnownValue b
    -> KnownValue c
    -> KnownValue d
    -> KnownValue e
    -> KnownValue f
    -> KnownValue g
    -> KnownValue h
    -> KnownValue i
map8 f knownValue1 knownValue2 knownValue3 knownValue4 knownValue5 knownValue6 knownValue7 knownValue8 =
    succeed f
        |> andMap knownValue1
        |> andMap knownValue2
        |> andMap knownValue3
        |> andMap knownValue4
        |> andMap knownValue5
        |> andMap knownValue6
        |> andMap knownValue7
        |> andMap knownValue8


{-| Make a known value that contains a value of a particular type but won't be encoded.
Primarily used to contain functions for `andMap`.
-}
succeed : a -> KnownValue a
succeed x =
    KnownValue x Types.ResponseIgnoreThis


{-| Create a known value that depends on another. Like `map2`, `map3`, and the other
map functions, this can be used to build larger known values.

For instance, this known value:
```
-- represents a column with a header,
-- as in a spreadsheet program
type Column a = Column String (List a)

knownIntColumn : Known (Column Int)
knownIntColumn =
    map2 Column
        (Known.string "Numbers")
        (Known.list
            [ Known.int 42
            , Known.int 37
            ]
        )
```

is equivalent to this one:
```
knownIntColumn : Known (Column Int)
knownIntColumn =
    Known.succeed Column
        |> Known.andMap (Known.string "Numbers")
        |> Known.andMap
            (Known.list
                [ Known.int 42
                , Known.int 37
                ]
            )
```
-}
andMap : KnownValue a -> KnownValue (a -> b) -> KnownValue b
andMap =
    map2 (|>)


{-| Query for a `KnownValue`.
-}
query : KnownValue a -> Query a
query (KnownValue x expectedQueryResponse) =
    Query
        { queryAST = Types.Known expectedQueryResponse
        , parser =
            (\queryResponse ->
                if queryResponse == expectedQueryResponse || expectedQueryResponse == Types.ResponseIgnoreThis then
                    Ok x
                else
                    Err (Types.ExpectedKnown expectedQueryResponse queryResponse)
            )
        }


{-| A `KnownValue` is simply a container around a `QueryResponse`. Use this function
to get the underlying `QueryResponse`.
-}
queryResponse : KnownValue a -> QueryResponse
queryResponse (KnownValue _ response) =
    response


{-| Get the underlying value out of a `KnownValue`.
-}
value : KnownValue a -> a
value (KnownValue x _) =
    x
