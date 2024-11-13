module VerifyExamples.DictAny.Foldr0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import DictAny exposing (..)
import DictAny as Dict exposing (Dict)

type alias User = { age : Int }

users : Dict String User
users =
    [ ("Fred", {age=33})
    , ("Bert", {age=19})
    , ("Adam", {age=42})
    ] |> Dict.fromList compare
getAges : Dict String User -> List Int
getAges dict =
    dict |> Dict.foldr consAge []
consAge : String -> User -> List Int -> List Int
consAge _ user ages =
    user.age :: ages



spec0 : Test.Test
spec0 =
    Test.test "#foldr: \n\n    getAges users\n    -->  [42,19,33]" <|
        \() ->
            Expect.equal
                (
                getAges users
                )
                (
                 [42,19,33]
                )