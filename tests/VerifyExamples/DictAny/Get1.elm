module VerifyExamples.DictAny.Get1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import DictAny exposing (..)
import DictAny as Dict exposing (Dict)

type Animal = Cat | Dog | Mouse

animals : Dict Animal String
animals = [ (Cat, "Tom"), (Mouse, "Jerry") ]
                |> fromList comparer
comparer : Animal -> Animal -> Order
comparer a b = compare (animalToInt a) (animalToInt b)
animalToInt : Animal -> Int
animalToInt animal =
    case animal of
        Cat -> 0
        Dog -> 1
        Mouse -> 2



spec1 : Test.Test
spec1 =
    Test.test "#get: \n\n    animals |> Dict.get comparer Dog\n    --> Nothing" <|
        \() ->
            Expect.equal
                (
                animals |> Dict.get comparer Dog
                )
                (
                Nothing
                )