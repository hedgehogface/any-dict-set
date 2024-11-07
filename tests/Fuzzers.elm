module Fuzzers exposing
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

import DictAny as Dict exposing (Dict)
import Fuzz exposing (Fuzzer)
import Fuzz.Unique


type alias Key =
    { id : Int, name : String }


comparer : Key -> Key -> Order
comparer a b =
    compare a.id b.id


type alias Value =
    Float


type KvNotInDict
    = KvNotInDict Key Value


type KvInDict
    = KvInDict Key Value


type DictTestValue
    = DictTestValue KvInDict (Dict Key Value) KvNotInDict


dictTestValueFuzzer : Fuzzer DictTestValue
dictTestValueFuzzer =
    Fuzz.map2
        (\( kn, vn ) kvs ->
            kvs
                |> List.map
                    (\( k, v ) ->
                        DictTestValue
                            (KvInDict k v)
                            (Dict.fromList comparer kvs)
                            (KvNotInDict kn vn)
                            |> Fuzz.constant
                    )
        )
        pairFuzzer
        (pairFuzzer |> Fuzz.listOfLengthBetween 1 20)
        |> Fuzz.andThen Fuzz.oneOf


fromOpsFuzzer : Fuzzer (Dict Key Value)
fromOpsFuzzer =
    opFuzzer
        |> Fuzz.listOfLengthBetween 0 100
        |> Fuzz.map (List.foldl applyOp Dict.empty)


applyOp : Op -> Dict Key Value -> Dict Key Value
applyOp op acc =
    case op of
        Insert ( k, v ) ->
            Dict.insert comparer k v acc

        Delete index ->
            let
                listed : List ( Key, Value )
                listed =
                    Dict.toList acc

                fixedIndex : Int
                fixedIndex =
                    -- the *2 makes it a 50% chance of deleting
                    -- the +1 avoids a division by zero
                    modBy (List.length listed * 2 + 1) index
            in
            case List.drop fixedIndex (Dict.keys acc) of
                key :: _ ->
                    Dict.remove comparer key acc

                _ ->
                    acc


opFuzzer : Fuzzer Op
opFuzzer =
    Fuzz.oneOf
        [ Fuzz.map Insert pairFuzzer
        , Fuzz.map Delete Fuzz.int
        ]


type Op
    = Insert ( Key, Value )
    | Delete Int



-- fromListFuzzer : Fuzzer (Dict Key Value)
-- fromListFuzzer =
--     pairListFuzzer
--         |> Fuzz.map (Dict.fromList comparer)
-- pairListFuzzer : Fuzzer (List (Key,Value))
-- pairListFuzzer =
--     pairFuzzer|>Fuzz.listOfLengthBetween 0 20


pairFuzzer : Fuzzer ( Key, Value )
pairFuzzer =
    Fuzz.map3
        (\id name value -> ( { id = id, name = name }, value ))
        Fuzz.Unique.int
        Fuzz.Unique.string
        Fuzz.Unique.float



-- keyFuzzer : Fuzzer Key
-- keyFuzzer =
--     Fuzz.map2
--         (\id name -> { id = id, name = name })
--         (Fuzz.intRange 1 20)
--         (Fuzz.asciiStringOfLength 5)


valueFuzzer : Fuzzer Value
valueFuzzer =
    Fuzz.niceFloat
