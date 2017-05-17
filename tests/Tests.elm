module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import Query exposing (Query)
import Query.Advanced as Query
import String.Extra as String
import Dict exposing (Dict)


all : Test
all =
    describe "All tests"
        [ primitiveTests
        , biggerStructureTests
        ]


primitiveTests : Test
primitiveTests =
    describe "Test query primitives"
        [ fuzz Fuzz.int
            "int fuzz"
            (\anInt ->
                Query.parseModel Query.int (toString anInt)
                    |> Expect.equal (Ok anInt)
            )
        , fuzz Fuzz.float
            "float fuzz"
            (\aFloat ->
                Query.parseModel Query.float (toString aFloat ++ "f")
                    |> Expect.equal (Ok aFloat)
            )
        , test "boolean true"
            (\_ ->
                Query.parseModel Query.bool "True"
                    |> Expect.equal (Ok True)
            )
        , test "boolean false"
            (\_ ->
                Query.parseModel Query.bool "False"
                    |> Expect.equal (Ok False)
            )
        , test "unit"
            (\_ ->
                Query.parseModel Query.unit "Unit"
                    |> Expect.equal (Ok ())
            )
        , fuzz Fuzz.string
            "string fuzz"
            (\aString ->
                Query.parseModel Query.string (toString aString)
                    |> Expect.equal (Ok (String.unquote (toString aString)))
            )
        ]


type Primitive
    = PrimitiveUnit
    | PrimitiveBool Bool
    | PrimitiveInt Int
    | PrimitiveFloat Float
    | PrimitiveString String


primitiveFuzzer : Fuzzer Primitive
primitiveFuzzer =
    Fuzz.frequencyOrCrash
        [ ( 1, Fuzz.constant PrimitiveUnit )
        , ( 1, Fuzz.bool |> Fuzz.map PrimitiveBool )
        , ( 1, Fuzz.int |> Fuzz.map PrimitiveInt )
        , ( 1, Fuzz.float |> Fuzz.map PrimitiveFloat )
        , ( 1, Fuzz.string |> Fuzz.map PrimitiveString )
        ]


queryForPrimitive : Primitive -> Query Primitive
queryForPrimitive prim =
    case prim of
        PrimitiveUnit ->
            Query.unit |> Query.map (\_ -> PrimitiveUnit)

        PrimitiveBool _ ->
            Query.bool |> Query.map PrimitiveBool

        PrimitiveInt _ ->
            Query.int |> Query.map PrimitiveInt

        PrimitiveFloat _ ->
            Query.float |> Query.map PrimitiveFloat

        PrimitiveString _ ->
            Query.string |> Query.map PrimitiveString


queryResponseForPrimitive : Primitive -> Query.QueryResponse
queryResponseForPrimitive prim =
    case prim of
        PrimitiveUnit ->
            Query.ResponseUnit

        PrimitiveBool b ->
            Query.ResponseBool b

        PrimitiveInt i ->
            Query.ResponseInt i

        PrimitiveFloat f ->
            Query.ResponseFloat f

        PrimitiveString s ->
            Query.ResponseString s


unquoteIfPrimitiveString : Primitive -> Primitive
unquoteIfPrimitiveString prim =
    case prim of
        PrimitiveString s ->
            toString s |> String.unquote |> PrimitiveString

        _ ->
            prim


primitiveQueryAndResponse : Primitive -> ( Primitive, Query Primitive, Query.QueryResponse )
primitiveQueryAndResponse prim =
    ( unquoteIfPrimitiveString prim, queryForPrimitive prim, queryResponseForPrimitive prim )


queryGenericPrimitive : Query Primitive
queryGenericPrimitive =
    Query.oneOf
        [ queryForPrimitive PrimitiveUnit
        , queryForPrimitive (PrimitiveBool False)
        , queryForPrimitive (PrimitiveInt 0)
        , queryForPrimitive (PrimitiveFloat 0)
        , queryForPrimitive (PrimitiveString "")
        ]


dictTest :
    String
    -> Fuzzer comparable
    -> (comparable -> comparable)
    -> (comparable -> Query.QueryResponse)
    -> Query comparable
    -> Test
dictTest description keyFuzzer keyTransform toResponse keyQuery =
    fuzz (Fuzz.list (Fuzz.tuple ( keyFuzzer, primitiveFuzzer )))
        description
        (\aList ->
            let
                asDict =
                    Dict.fromList aList

                uniquifiedByKey =
                    Dict.toList asDict

                expected =
                    List.map
                        (\( key, value ) ->
                            ( keyTransform key, unquoteIfPrimitiveString value )
                        )
                        uniquifiedByKey
                        |> Dict.fromList
            in
                uniquifiedByKey
                    |> List.map
                        (\( key, value ) ->
                            ( toResponse key, queryResponseForPrimitive value )
                        )
                    |> Query.ResponseDict
                    |> Query.queryResponseToString
                    |> Query.parseModel (Query.dict keyQuery queryGenericPrimitive)
                    |> Expect.equal (Ok expected)
        )


biggerStructureTests : Test
biggerStructureTests =
    describe "Test bigger query structures"
        [ fuzz2
            (Fuzz.map primitiveQueryAndResponse primitiveFuzzer)
            (Fuzz.map primitiveQueryAndResponse primitiveFuzzer)
            "pair fuzz"
            (\( prim1, query1, resp1 ) ( prim2, query2, resp2 ) ->
                Query.ResponseProduct resp1 resp2
                    |> Query.queryResponseToString
                    |> Query.parseModel (Query.pair query1 query2)
                    |> Expect.equal (Ok ( prim1, prim2 ))
            )
        , fuzz2
            (Fuzz.map primitiveQueryAndResponse primitiveFuzzer)
            (Fuzz.map primitiveQueryAndResponse primitiveFuzzer)
            "oneOf first fuzz"
            (\( prim1, query1, resp1 ) ( prim2, query2, resp2 ) ->
                Query.queryResponseToString resp1
                    |> Query.parseModel (Query.oneOf [ query1, query2 ])
                    |> Expect.equal (Ok prim1)
            )
        , fuzz2
            (Fuzz.map primitiveQueryAndResponse primitiveFuzzer)
            (Fuzz.map primitiveQueryAndResponse primitiveFuzzer)
            "oneOf second fuzz"
            (\( prim1, query1, resp1 ) ( prim2, query2, resp2 ) ->
                Query.queryResponseToString resp2
                    |> Query.parseModel (Query.oneOf [ query1, query2 ])
                    |> Expect.equal (Ok prim2)
            )
        , fuzz (Fuzz.list primitiveFuzzer)
            "primitive list fuzz"
            (\aList ->
                List.map queryResponseForPrimitive aList
                    |> Query.ResponseList
                    |> Query.queryResponseToString
                    |> Query.parseModel (Query.list queryGenericPrimitive)
                    |> Expect.equal (Ok (List.map unquoteIfPrimitiveString aList))
            )
        , fuzz2
            (Fuzz.list primitiveFuzzer)
            (Fuzz.list primitiveFuzzer)
            "pair list fuzz"
            (\aList bList ->
                List.map2 Query.ResponseProduct
                    (List.map queryResponseForPrimitive aList)
                    (List.map queryResponseForPrimitive bList)
                    |> Query.ResponseList
                    |> Query.queryResponseToString
                    |> Query.parseModel (Query.list (Query.pair queryGenericPrimitive queryGenericPrimitive))
                    |> Expect.equal
                        (Ok <|
                            List.map2 (,)
                                (List.map unquoteIfPrimitiveString aList)
                                (List.map unquoteIfPrimitiveString bList)
                        )
            )
        , dictTest "string dict fuzz"
            Fuzz.string
            (toString >> String.unquote)
            Query.ResponseString
            Query.string
        , dictTest "int dict fuzz"
            Fuzz.int
            identity
            Query.ResponseInt
            Query.int
        , dictTest "float dict fuzz"
            Fuzz.float
            identity
            Query.ResponseFloat
            Query.float
        ]
