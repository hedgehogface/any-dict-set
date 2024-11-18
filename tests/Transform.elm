module Transform exposing (suite)

import Common exposing (expectEqual)
import DictAny as Dict
import Expect
import Fuzzers exposing (DictTestValue(..), Key, Value, comparer, dictTestValueFuzzer)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "transform"
        [ mapTest
        , foldlTest
        , foldrTest
        , filterTest
        , partitionTest
        ]


mapTest : Test
mapTest =
    let
        f1 : Key -> Value -> Value
        f1 { id } v =
            v + toFloat id

        f2 : Key -> Value -> Value
        f2 _ v =
            v + 1

        tests : List Test
        tests =
            [ ( "f1", f1 )
            , ( "f2", f2 )
            ]
                |> List.map
                    (\( flabel, f ) ->
                        [ fuzz dictTestValueFuzzer "Is equivalent to mapping on the list" <|
                            \(DictTestValue _ dict _) ->
                                dict
                                    |> Dict.map f
                                    |> expectEqual
                                        (dict
                                            |> Dict.toList
                                            |> List.map (\( k, v ) -> ( k, f k v ))
                                            |> Dict.fromList comparer
                                        )
                        , fuzz dictTestValueFuzzer "Doesn't change the size" <|
                            \(DictTestValue _ dict _) ->
                                dict
                                    |> Dict.map f
                                    |> Dict.size
                                    |> Expect.equal (Dict.size dict)
                        ]
                            |> describe flabel
                    )
    in
    describe "map"
        (tests
            ++ [ fuzz dictTestValueFuzzer "map (always identity) == identity" <|
                    \(DictTestValue _ dict _) ->
                        dict
                            |> Dict.map (always identity)
                            |> expectEqual dict
               ]
        )


foldlTest : Test
foldlTest =
    describe "foldl"
        [ fuzz dictTestValueFuzzer "foldl (::) is equivalent to toList >> reverse" <|
            \(DictTestValue _ dict _) ->
                dict
                    |> Dict.foldl (\k v -> (::) ( k, v )) []
                    |> Expect.equalLists (List.reverse <| Dict.toList dict)
        , fuzz dictTestValueFuzzer "foldl insert is an identity" <|
            \(DictTestValue _ dict _) ->
                dict
                    |> Dict.foldl (Dict.insert comparer) Dict.empty
                    |> expectEqual dict
        ]


foldrTest : Test
foldrTest =
    describe "foldr"
        [ fuzz dictTestValueFuzzer "foldr (::) is equivalent to toList" <|
            \(DictTestValue _ dict _) ->
                dict
                    |> Dict.foldr (\k v -> (::) ( k, v )) []
                    |> Expect.equalLists (Dict.toList dict)
        , fuzz dictTestValueFuzzer "foldr insert is an identity" <|
            \(DictTestValue _ dict _) ->
                dict
                    |> Dict.foldr (Dict.insert comparer) Dict.empty
                    |> expectEqual dict
        ]


filterTest : Test
filterTest =
    let
        f : Key -> Value -> Bool
        f _ v =
            modBy 2 (floor v) == 0
    in
    describe "filter"
        [ fuzz dictTestValueFuzzer "Is equivalent to toList >> List.filter >> fromList" <|
            \(DictTestValue _ dict _) ->
                dict
                    |> Dict.filter comparer f
                    |> expectEqual
                        (dict
                            |> Dict.toList
                            |> List.filter (\( k, v ) -> f k v)
                            |> Dict.fromList comparer
                        )
        ]


partitionTest : Test
partitionTest =
    let
        f : Key -> Value -> Bool
        f _ v =
            modBy 2 (floor v) == 0
    in
    describe "partition"
        [ fuzz dictTestValueFuzzer "Is equivalent to toList >> List.partition >> fromList" <|
            \(DictTestValue _ dict _) ->
                let
                    ( l, r ) =
                        Dict.partition comparer f dict

                    ( el, er ) =
                        dict
                            |> Dict.toList
                            |> List.partition (\( k, v ) -> f k v)
                            |> Tuple.mapBoth (Dict.fromList comparer) (Dict.fromList comparer)
                in
                Expect.all
                    [ \_ -> expectEqual el l
                    , \_ -> expectEqual er r
                    ]
                    ()
        ]
