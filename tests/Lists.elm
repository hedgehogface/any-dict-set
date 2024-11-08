module Lists exposing (suite)

import DictAny as Dict
import Expect
import Fuzz
import Fuzzers exposing (DictTestValue(..), Key, Value, comparer, dictTestValueFuzzer, pairFuzzer)
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
        , fuzz dictTestValueFuzzer "Is sorted" <|
            \(DictTestValue _ dict _) ->
                let
                    keys : List Key
                    keys =
                        Dict.keys dict
                in
                keys
                    |> Expect.equal (List.sortWith comparer keys)
        , fuzz dictTestValueFuzzer "Contains no duplicates" <|
            \(DictTestValue _ dict _) ->
                let
                    keys : List Key
                    keys =
                        Dict.keys dict
                in
                keys
                    |> Expect.equal (dedupBy .id <| List.sortWith comparer keys)
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


sorter : ( Key, Value ) -> ( Key, Value ) -> Order
sorter =
    \( a, _ ) ( b, _ ) -> comparer a b


toListTest : Test
toListTest =
    describe "toList"
        [ fuzz dictTestValueFuzzer "Is sorted by key" <|
            \(DictTestValue _ dict _) ->
                dict
                    |> Dict.toList
                    |> List.sortWith sorter
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
            pairFuzzer |> Fuzz.listOfLengthBetween 1 100
    in
    describe "fromList"
        [ fuzz pairListFuzzer "Combined with toList is the equivalent of sort >> dedupBy Tuple.first" <|
            \list ->
                list
                    |> Dict.fromList comparer
                    |> Dict.toList
                    |> Expect.equalLists (dedupBy (Tuple.first >> .id) (List.sortWith sorter list))
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
