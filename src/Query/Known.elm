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
        )

{-|
@docs KnownValue

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


{-| TODO
-}
type KnownValue a
    = KnownValue a QueryResponse


primitive : (a -> QueryResponse) -> a -> KnownValue a
primitive queryResponseConstructor x =
    KnownValue x (queryResponseConstructor x)


{-| TODO
-}
int : Int -> KnownValue Int
int =
    primitive Types.ResponseInt


{-| TODO
-}
float : Float -> KnownValue Float
float =
    primitive Types.ResponseFloat


{-| TODO
-}
bool : Bool -> KnownValue Bool
bool =
    primitive Types.ResponseBool


{-| TODO
-}
string : String -> KnownValue String
string =
    primitive Types.ResponseString


{-| TODO
-}
unit : KnownValue ()
unit =
    KnownValue () (Types.ResponseUnit)


{-| TODO
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


{-| TODO
-}
map : (a -> b) -> KnownValue a -> KnownValue b
map f (KnownValue x queryResponse) =
    KnownValue (f x) queryResponse


{-| TODO
-}
map2 :
    (a -> b -> c)
    -> KnownValue a
    -> KnownValue b
    -> KnownValue c
map2 f (KnownValue value1 queryResponse1) (KnownValue value2 queryResponse2) =
    KnownValue (f value1 value2) (Types.ResponseProduct queryResponse1 queryResponse2)


{-| TODO
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


{-| TODO
-}
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


{-| TODO
-}
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


{-| TODO
-}
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


{-| TODO
-}
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


{-| TODO
-}
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


{-| TODO
-}
succeed : a -> KnownValue a
succeed x =
    KnownValue x Types.ResponseIgnoreThis


{-| TODO
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


{-| TODO
-}
queryResponse : KnownValue a -> QueryResponse
queryResponse (KnownValue _ response) =
    response
