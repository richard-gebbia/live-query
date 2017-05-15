module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import Query exposing (Query)
import Query.Advanced as Query
import String.Extra as String


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


primitiveFuzzer : Fuzzer ( Primitive, Query Primitive, Query.QueryResponse )
primitiveFuzzer =
    Fuzz.frequencyOrCrash
        [ ( 1, Fuzz.constant ( PrimitiveUnit, Query.unit |> Query.map (\_ -> PrimitiveUnit), Query.ResponseUnit ) )
        , ( 1, Fuzz.bool |> Fuzz.map (\b -> ( PrimitiveBool b, Query.bool |> Query.map PrimitiveBool, Query.ResponseBool b )) )
        , ( 1, Fuzz.int |> Fuzz.map (\i -> ( PrimitiveInt i, Query.int |> Query.map PrimitiveInt, Query.ResponseInt i )) )
        , ( 1, Fuzz.float |> Fuzz.map (\f -> ( PrimitiveFloat f, Query.float |> Query.map PrimitiveFloat, Query.ResponseFloat f )) )
        , ( 1, Fuzz.string |> Fuzz.map (\s -> ( PrimitiveString (String.unquote (toString s)), Query.string |> Query.map PrimitiveString, Query.ResponseString s )) )
        ]


biggerStructureTests : Test
biggerStructureTests =
    describe "Test bigger query structures"
        [ fuzz2
            primitiveFuzzer
            primitiveFuzzer
            "pair fuzz"
            (\( prim1, query1, resp1 ) ( prim2, query2, resp2 ) ->
                Query.ResponseProduct resp1 resp2
                    |> Query.queryResponseToString
                    |> Query.parseModel (Query.pair query1 query2)
                    |> Expect.equal (Ok ( prim1, prim2 ))
            )
        , fuzz2
            primitiveFuzzer
            primitiveFuzzer
            "oneOf first fuzz"
            (\( prim1, query1, resp1 ) ( prim2, query2, resp2 ) ->
                Query.queryResponseToString resp1
                    |> Query.parseModel (Query.oneOf [ query1, query2 ])
                    |> Expect.equal (Ok prim1)
            )
        , fuzz2
            primitiveFuzzer
            primitiveFuzzer
            "oneOf second fuzz"
            (\( prim1, query1, resp1 ) ( prim2, query2, resp2 ) ->
                Query.queryResponseToString resp2
                    |> Query.parseModel (Query.oneOf [ query1, query2 ])
                    |> Expect.equal (Ok prim2)
            )
        ]
