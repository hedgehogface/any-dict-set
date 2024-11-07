module Lists exposing (suite)

import DictAny as Dict
import Expect
import Fuzz
import Fuzzers exposing (DictTestValue(..), Key, comparer, dictTestValueFuzzer, pairFuzzer)
import List.Extra
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "lists"
        [ keysTest
        , valuesTest
        , toListTest
        , fromListTest
        ]


keysTest : Test
keysTest =
    describe "keys"
        [ fuzz dictTestValueFuzzer "Is equivalent to List.map Tuple.first toList" <|
            \(DictTestValue _ dict _) ->
                dict
                    |> Dict.keys
                    |> Expect.equalLists (List.map Tuple.first (Dict.toList dict))
        , fuzz dictTestValueFuzzer "Has the correct size" <|
            \(DictTestValue _ dict _) ->
                Dict.keys dict
                    |> List.length
                    |> Expect.equal (Dict.size dict)

        -- , fuzz dictTestValueFuzzer "Is sorted" <|
        --     \(DictTestValue _ dict _) ->
        --         let
        --             keys : List Key
        --             keys =
        --                 Dict.keys dict
        --         in
        --         keys
        --             |> Expect.equal (List.sort keys)
        , fuzz dictTestValueFuzzer "Contains no duplicates" <|
            \(DictTestValue _ dict _) ->
                let
                    keys : List Key
                    keys =
                        Dict.keys dict

                    uniqueKeys : List Key
                    uniqueKeys =
                        keys |> List.Extra.uniqueBy .id
                in
                keys |> Expect.equal uniqueKeys
        ]


valuesTest : Test
valuesTest =
    describe "values"
        [ fuzz dictTestValueFuzzer "Is equivalent to List.map Tuple.second toList" <|
            \(DictTestValue _ dict _) ->
                dict
                    |> Dict.values
                    |> Expect.equalLists (List.map Tuple.second (Dict.toList dict))
        , fuzz dictTestValueFuzzer "Has the correct size" <|
            \(DictTestValue _ dict _) ->
                Dict.values dict
                    |> List.length
                    |> Expect.equal (Dict.size dict)
        ]


toListTest : Test
toListTest =
    describe "toList"
        [ fuzz dictTestValueFuzzer "Is sorted by key" <|
            \(DictTestValue _ dict _) ->
                dict
                    |> Dict.toList
                    |> Expect.equalLists (Dict.toList dict)
        , fuzz dictTestValueFuzzer "Has the correct size" <|
            \(DictTestValue _ dict _) ->
                Dict.toList dict
                    |> List.length
                    |> Expect.equal (Dict.size dict)
        ]


fromListTest : Test
fromListTest =
    let
        pairListFuzzer =
            pairFuzzer |> Fuzz.listOfLengthBetween 1 20
    in
    describe "fromList"
        [ fuzz pairListFuzzer "Combined with toList is the equivalent of sort >> dedupBy Tuple.first" <|
            \list ->
                list
                    |> Dict.fromList comparer
                    |> Dict.toList
                    |> Expect.equalLists list
        , fuzz dictTestValueFuzzer "Is the inverse to toList" <|
            \(DictTestValue _ dict _) ->
                dict
                    |> Dict.toList
                    |> Dict.fromList comparer
                    |> Dict.toList
                    |> Expect.equal (Dict.toList dict)
        ]


dedupBy : (a -> b) -> List a -> List a
dedupBy f =
    List.foldr
        (\e acc ->
            case acc of
                [] ->
                    [ e ]

                last :: _ ->
                    if f e == f last then
                        acc

                    else
                        e :: acc
        )
        []
