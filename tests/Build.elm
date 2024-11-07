module Build exposing (singletonTest, suite)

import Common exposing (expectEqual)
import DictAny as Dict exposing (Dict)
import Expect
import Fuzz exposing (Fuzzer)
import Fuzzers
    exposing
        ( DictTestValue(..)
        , Key
        , KvInDict(..)
        , KvNotInDict(..)
        , Value
        , comparer
        , dictTestValueFuzzer
        , pairFuzzer
        , valueFuzzer
        )
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "build"
        [ emptyTest
        , singletonTest
        , insertTest
        , updateTest
        , removeTest
        ]


emptyTest : Test
emptyTest =
    describe "empty"
        [ test "Has size 0" <|
            \_ ->
                Dict.empty
                    |> Dict.size
                    |> Expect.equal 0
        , fuzz pairFuzzer "Does not contain any element" <|
            \( key, _ ) ->
                Dict.member comparer key Dict.empty
                    |> Expect.equal False
        , test "Has an empty toList" <|
            \_ ->
                Dict.empty
                    |> Dict.toList
                    |> Expect.equalLists []
        ]


singletonTest : Test
singletonTest =
    let
        key : Key
        key =
            { id = 0, name = "singleton" }

        value : Value
        value =
            1

        singleton : Dict Key Value
        singleton =
            Dict.singleton key value
    in
    describe "singleton"
        [ test "Has size 1" <|
            \_ ->
                singleton
                    |> Dict.size
                    |> Expect.equal 1
        , fuzz pairFuzzer "Only contains its key" <|
            \( k, _ ) ->
                Dict.member comparer k singleton
                    |> Expect.equal (k == key)
        , test "Has a singleton toList" <|
            \_ ->
                singleton
                    |> Dict.toList
                    |> Expect.equalLists (List.singleton ( key, value ))
        ]


insertTest : Test
insertTest =
    let
        insertFuzzer : Fuzzer DictTestValue
        insertFuzzer =
            dictTestValueFuzzer
    in
    describe "insert"
        [ fuzz insertFuzzer "Allows using get to return the same value" <|
            \(DictTestValue _ dict (KvNotInDict key value)) ->
                Dict.get comparer key (Dict.insert comparer key value dict)
                    |> Expect.equal (Just value)
        , fuzz insertFuzzer "Increases size by 0 (resp. 1) if already a member (resp. if not)" <|
            \(DictTestValue _ dict (KvNotInDict key value)) ->
                let
                    increment : Int
                    increment =
                        if Dict.member comparer key dict then
                            0

                        else
                            1
                in
                Dict.size (Dict.insert comparer key value dict)
                    |> Expect.equal (Dict.size dict + increment)
        , fuzz2 insertFuzzer valueFuzzer "Overwrites existing values" <|
            \(DictTestValue _ dict (KvNotInDict key value)) value2 ->
                dict
                    |> Dict.insert comparer key value
                    |> Dict.insert comparer key value2
                    |> Dict.get comparer key
                    |> Expect.equal (Just value2)
        ]


updateTest : Test
updateTest =
    let
        updateFuzzer : Fuzzer DictTestValue
        updateFuzzer =
            dictTestValueFuzzer
    in
    describe "update"
        {- These tests use `Expect.equal` which would normally be too strict,
           but since in the future `update` could be rewritten by melding get/insert/delete
           we want to make sure that the structure is correctly preserved.
        -}
        [ fuzz updateFuzzer "update k (\\_ -> Nothing) is equivalent to remove k" <|
            \(DictTestValue (KvInDict key _) dict _) ->
                dict
                    |> Dict.update comparer key (\_ -> Nothing)
                    |> expectEqual (Dict.remove comparer key dict)
        , fuzz2 updateFuzzer valueFuzzer "update k (\\_ -> Just v) is equivalent to insert k v" <|
            \(DictTestValue (KvInDict key _) dict _) value2 ->
                dict
                    |> Dict.update comparer key (\_ -> Just value2)
                    |> expectEqual (Dict.insert comparer key value2 dict)
        , fuzz updateFuzzer "update k identity is equivalent to identity" <|
            \(DictTestValue (KvInDict key _) dict _) ->
                dict
                    |> Dict.update comparer key identity
                    |> expectEqual dict
        ]


removeTest : Test
removeTest =
    let
        removeFuzzer : Fuzzer DictTestValue
        removeFuzzer =
            dictTestValueFuzzer
    in
    describe "remove"
        [ fuzz removeFuzzer "Will make sure a key is not present after deletion" <|
            \(DictTestValue (KvInDict key _) dict _) ->
                Dict.get comparer key (Dict.remove comparer key dict)
                    |> Expect.equal Nothing
        , fuzz removeFuzzer "Decreases size by 1 (resp. 0) if a member (resp. if not)" <|
            \(DictTestValue (KvInDict key _) dict _) ->
                let
                    decrement : Int
                    decrement =
                        if Dict.member comparer key dict then
                            1

                        else
                            0
                in
                Dict.size (Dict.remove comparer key dict)
                    |> Expect.equal (Dict.size dict - decrement)
        , fuzz removeFuzzer "Doesn't touch the dictionary if the key is not present" <|
            \(DictTestValue (KvInDict key _) dict _) ->
                (Dict.remove comparer key dict == dict)
                    |> Expect.equal (not (Dict.member comparer key dict))
        ]
