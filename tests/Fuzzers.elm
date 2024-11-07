module Fuzzers exposing (Key, Value, comparer, dictFuzzer, keyFuzzer, pairListFuzzer, valueFuzzer)

import DictAny as Dict exposing (Dict)
import Fuzz exposing (Fuzzer)
import Fuzz.Unique


type alias Key =
    { id : Int, name : String }


comparer : Key -> Key -> Order
comparer a b =
    compare a.id b.id


type alias Value =
    Int

type alias DictTestValue =
    { kv_in_dict : (Key,Value)
    , dict : Dict Key Value 
    , kv_not_in_dict : (Key,Value)
    }


dictFuzzer : Fuzzer DictTestValue
dictFuzzer =
    Fuzz.map2
        (\k v -> ( k, v ))
        keyFuzzer
        valueFuzzer
        |> Fuzz.listOfLengthBetween 0 10
        |> Fuzz.map (Dict.fromList comparer)


fromOpsFuzzer : Fuzzer (Dict Key Value)
fromOpsFuzzer =
    opFuzzer
        |> Fuzz.listOfLengthBetween 0 100
        |> Fuzz.map (List.foldl applyOp Dict.empty)


applyOp : Op -> Dict Key Value -> Dict Key Value
applyOp op acc =
    case op of
        Insert k v ->
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
        [ Fuzz.map2 Insert keyFuzzer valueFuzzer
        , Fuzz.map Delete Fuzz.int
        ]


type Op
    = Insert Key Value
    | Delete Int


fromListFuzzer : Fuzzer (Dict Key Value)
fromListFuzzer =
    pairListFuzzer
        |> Fuzz.map (Dict.fromList comparer)


pairListFuzzer : Fuzzer (List ( Key, Value ))
pairListFuzzer =
    Fuzz.pair keyFuzzer valueFuzzer
        |> Fuzz.listOfLengthBetween 1 100


keyFuzzer : Fuzzer Key
keyFuzzer =
    Fuzz.map2
        (\id name -> { id = id, name = name })
        (Fuzz.intRange 1 20)
        (Fuzz.asciiStringOfLength 5)


valueFuzzer : Fuzzer Value
valueFuzzer =
    Fuzz.niceFloat
