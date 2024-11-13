module Combine exposing (suite)

import DictAny as Dict exposing (Dict)
import Expect exposing (FloatingPointTolerance(..))
import Fuzz
import Fuzzers
    exposing
        ( DictTestValue(..)
        , Key
        , KvInDict(..)
        , KvNotInDict(..)
        , Value
        , comparer
        , dictTestValueFuzzer
        )
import Test exposing (Test, describe, fuzz, fuzz2)


suite : Test
suite =
    describe "combine"
        [ unionTest
        , intersectTest
        , diffTest
        , mergeTest
        ]


expectValueEquals : Value -> Value -> Expect.Expectation
expectValueEquals v1 v2 =
    v1 |> Expect.within (Absolute 0.00000001) v2


unionTest : Test
unionTest =
    let
        unionFuzzer : Fuzz.Fuzzer ( DictTestValue, DictTestValue )
        unionFuzzer =
            Fuzz.pair dictTestValueFuzzer dictTestValueFuzzer
    in
    describe "union"
        [ fuzz unionFuzzer "Contains the correct values giving preference to the dict1" <|
            \( DictTestValue (KvInDict key _) dict1 _, DictTestValue _ dict2 _ ) ->
                case
                    ( Dict.get comparer key dict1
                    , Dict.get comparer key dict2
                    , Dict.get comparer key (Dict.union comparer dict1 dict2)
                    )
                of
                    ( Just fvalue, _, Just uvalue ) ->
                        uvalue |> expectValueEquals fvalue

                    ( Nothing, Just svalue, Just uvalue ) ->
                        uvalue |> expectValueEquals svalue

                    ( Nothing, Nothing, Nothing ) ->
                        Expect.pass

                    ( Just _, _, Nothing ) ->
                        Expect.fail "Value found in dict1 but not in union"

                    ( _, Just _, Nothing ) ->
                        Expect.fail "Value found in dict2 but not in union"

                    ( Nothing, Nothing, Just _ ) ->
                        Expect.fail "Value found in union but not in dict1 nor dict2"
        ]


intersectTest : Test
intersectTest =
    let
        intersectFuzzer : Fuzz.Fuzzer ( DictTestValue, DictTestValue )
        intersectFuzzer =
            Fuzz.pair dictTestValueFuzzer dictTestValueFuzzer
    in
    describe "intersect"
        [ fuzz intersectFuzzer "Contains the correct values giving preference to the dict1" <|
            \( DictTestValue (KvInDict key _) dict1 _, DictTestValue _ dict2 _ ) ->
                case
                    ( Dict.get comparer key dict1
                    , Dict.get comparer key dict2
                    , Dict.get comparer key (Dict.intersect comparer dict1 dict2)
                    )
                of
                    ( Just fvalue, Just _, Just uvalue ) ->
                        uvalue |> Expect.equal fvalue

                    ( Nothing, _, Nothing ) ->
                        Expect.pass

                    ( _, Nothing, Nothing ) ->
                        Expect.pass

                    ( Nothing, _, Just _ ) ->
                        Expect.fail "Value found in intersection but not in dict1"

                    ( _, Nothing, Just _ ) ->
                        Expect.fail "Value found in intersection but not in dict2"

                    ( Just _, Just _, Nothing ) ->
                        Expect.fail "Value found in both but not in intersection"
        ]


diffTest : Test
diffTest =
    let
        diffFuzzer : Fuzz.Fuzzer ( DictTestValue, DictTestValue )
        diffFuzzer =
            Fuzz.pair dictTestValueFuzzer dictTestValueFuzzer
    in
    describe "diff"
        [ fuzz diffFuzzer "Contains the correct values giving preference to the dict1" <|
            \( DictTestValue (KvInDict key _) dict1 _, DictTestValue _ dict2 _ ) ->
                let
                    diff : Dict Key Value
                    diff =
                        Dict.diff comparer dict1 dict2

                    got : Maybe Value
                    got =
                        Dict.get comparer key diff
                in
                if got == Nothing then
                    -- This is checked in the other test
                    Expect.pass

                else
                    got
                        |> Expect.equal (Dict.get comparer key dict1)
        , fuzz diffFuzzer "Contains the correct keys" <|
            \( DictTestValue (KvInDict key _) dict1 _, DictTestValue _ dict2 _ ) ->
                Dict.member comparer key (Dict.diff comparer dict1 dict2)
                    |> Expect.equal (Dict.member comparer key dict1 && not (Dict.member comparer key dict2))
        ]


mergeTest : Test
mergeTest =
    describe "merge"
        [ fuzz2 dictTestValueFuzzer dictTestValueFuzzer "Correctly categorizes elements" <|
            \(DictTestValue (KvInDict key _) left _) (DictTestValue _ right _) ->
                let
                    ( mergedL, mergedB, mergedR ) =
                        Dict.merge comparer
                            (\lk lv ( l, b, r ) -> ( ( lk, lv ) :: l, b, r ))
                            (\bk lv rv ( l, b, r ) -> ( l, ( bk, lv, rv ) :: b, r ))
                            (\rk rv ( l, b, r ) -> ( l, b, ( rk, rv ) :: r ))
                            left
                            right
                            ( [], [], [] )

                    ( lMember, rMember ) =
                        ( Dict.member comparer key left, Dict.member comparer key right )
                in
                if Dict.isEmpty left && Dict.isEmpty right then
                    Expect.all
                        [ \_ -> List.isEmpty mergedL |> Expect.equal True
                        , \_ -> List.isEmpty mergedB |> Expect.equal True
                        , \_ -> List.isEmpty mergedR |> Expect.equal True
                        ]
                        ()

                else
                    Expect.all
                        [ \_ ->
                            List.any (\( lk, _ ) -> lk == key) mergedL
                                |> Expect.equal (lMember && not rMember)
                        , \_ ->
                            List.any (\( bk, _, _ ) -> bk == key) mergedB
                                |> Expect.equal (lMember && rMember)
                        , \_ ->
                            List.any (\( rk, _ ) -> rk == key) mergedR
                                |> Expect.equal (not lMember && rMember)
                        ]
                        ()
        ]
