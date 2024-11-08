module Query exposing (suite)

import DictAny as Dict
import Expect
import Fuzzers
    exposing
        ( DictTestValue(..)
        , KvInDict(..)
        , Value
        , comparer
        , dictTestValueFuzzer
        , pairFuzzer
        )
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "query"
        [ isEmptyTest
        , memberTest
        , getTest
        , sizeTest
        ]


isEmptyTest : Test
isEmptyTest =
    describe "isEmpty"
        [ fuzz dictTestValueFuzzer "Is true iff the dict is the empty one" <|
            \(DictTestValue _ dict _) ->
                Dict.isEmpty dict
                    |> Expect.equal (dict == Dict.empty)
        ]


memberTest : Test
memberTest =
    describe "member"
        [ fuzz dictTestValueFuzzer "Is true iff get is not Nothing" <|
            \(DictTestValue (KvInDict key _) dict _) ->
                Dict.member comparer key dict
                    |> Expect.equal (Dict.get comparer key dict /= Nothing)
        , fuzz dictTestValueFuzzer "Is equivalent to List.member on the keys" <|
            \(DictTestValue (KvInDict key _) dict _) ->
                Dict.member comparer key dict
                    |> Expect.equal (List.member key (Dict.keys dict))
        ]


getTest : Test
getTest =
    describe "get"
        [ fuzz pairFuzzer "Retrieves a value from a singleton" <|
            \( key, value ) ->
                Dict.get comparer key (Dict.singleton key value)
                    |> Expect.equal (Just value)
        , fuzz pairFuzzer "Retrieves nothing from empty" <|
            \( key, _ ) ->
                Dict.get comparer key Dict.empty
                    |> Expect.equal Nothing
        , fuzz dictTestValueFuzzer "Value is found" <|
            \(DictTestValue (KvInDict key value) dict _) ->
                Dict.get comparer key dict
                    |> Expect.equal (Just value)
        ]


sizeTest : Test
sizeTest =
    describe "size"
        [ fuzz dictTestValueFuzzer "Is never negative" <|
            \(DictTestValue _ dict _) ->
                Dict.size dict
                    |> Expect.greaterThan -1
        , test "Is zero for Dict.empty" <|
            \_ ->
                Dict.size Dict.empty
                    |> Expect.equal 0
        , test "Is one for Dict.singleton" <|
            \_ ->
                Dict.size (Dict.singleton 0 0)
                    |> Expect.equal 1
        , fuzz dictTestValueFuzzer "Is zero iff the dictionary is empty" <|
            \(DictTestValue _ dict _) ->
                (Dict.size dict == 0)
                    |> Expect.equal (Dict.isEmpty dict)
        ]
