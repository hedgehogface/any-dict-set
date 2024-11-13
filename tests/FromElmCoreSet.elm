module FromElmCoreSet exposing (tests)

import Basics exposing (..)
import Expect
import List exposing ((::))
import SetAny as Set exposing (Set)
import Test exposing (..)


tests : Test
tests =
    describe "Set Tests"
        [ describe "empty" emptyTests
        , describe "singleton" singletonTests
        , describe "insert" insertTests
        , describe "remove" removeTests
        , describe "isEmpty" isEmptyTests
        , describe "member" memberTests
        , describe "size" sizeTests
        , describe "foldl" foldlTests
        , describe "foldr" foldrTests
        , describe "map" mapTests
        , describe "filter" filterTests
        , describe "partition" partitionTests
        , describe "union" unionTests
        , describe "intersect" intersectTests
        , describe "diff" diffTests
        , describe "toList" toListTests
        , describe "fromList" fromListTests
        ]



-- HELPERS


set42 : Set Int
set42 =
    Set.singleton 42



-- set1To3 : Set Int
-- set1To3 =
--     Set.fromList compare (List.range 1 3)


set1To100 : Set Int
set1To100 =
    Set.fromList compare (List.range 1 100)


set1To50 : Set Int
set1To50 =
    Set.fromList compare (List.range 1 50)


set51To100 : Set Int
set51To100 =
    Set.fromList compare (List.range 51 100)


set51To150 : Set Int
set51To150 =
    Set.fromList compare (List.range 51 150)


isLessThan51 : Int -> Bool
isLessThan51 n =
    n < 51



-- TESTS


emptyTests : List Test
emptyTests =
    [ test "returns an empty set" <|
        \() -> Expect.equal 0 (Set.size Set.empty)
    ]


singletonTests : List Test
singletonTests =
    [ test "returns set with one element" <|
        \() -> Expect.equal 1 (Set.size (Set.singleton 1))
    , test "contains given element" <|
        \() -> Expect.equal True (Set.member compare 1 (Set.singleton 1))
    ]


insertTests : List Test
insertTests =
    [ test "adds new element to empty set" <|
        \() -> Expect.equal set42 (Set.insert compare 42 Set.empty)
    , test "adds new element to a set of 100" <|
        \() -> Expect.equal (Set.fromList compare (List.range 1 101)) (Set.insert compare 101 set1To100)
    , test "leaves singleton set intact if it contains given element" <|
        \() -> Expect.equal set42 (Set.insert compare 42 set42)
    , test "leaves set of 100 intact if it contains given element" <|
        \() -> Expect.equal set1To100 (Set.insert compare 42 set1To100)
    ]


removeTests : List Test
removeTests =
    [ test "removes element from singleton set" <|
        \() -> Expect.equal Set.empty (Set.remove compare 42 set42)
    , test "removes element from set of 100" <|
        \() -> Expect.equal (Set.fromList compare (List.range 1 99)) (Set.remove compare 100 set1To100)
    , test "leaves singleton set intact if it doesn't contain given element" <|
        \() -> Expect.equal set42 (Set.remove compare -1 set42)
    , test "leaves set of 100 intact if it doesn't contain given element" <|
        \() -> Expect.equal set1To100 (Set.remove compare -1 set1To100)
    ]


isEmptyTests : List Test
isEmptyTests =
    [ test "returns True for empty set" <|
        \() -> Expect.equal True (Set.isEmpty Set.empty)
    , test "returns False for singleton set" <|
        \() -> Expect.equal False (Set.isEmpty set42)
    , test "returns False for set of 100" <|
        \() -> Expect.equal False (Set.isEmpty set1To100)
    ]


memberTests : List Test
memberTests =
    [ test "returns True when given element inside singleton set" <|
        \() -> Expect.equal True (Set.member compare 42 set42)
    , test "returns True when given element inside set of 100" <|
        \() -> Expect.equal True (Set.member compare 42 set1To100)
    , test "returns False for element not in singleton" <|
        \() -> Expect.equal False (Set.member compare -1 set42)
    , test "returns False for element not in set of 100" <|
        \() -> Expect.equal False (Set.member compare -1 set1To100)
    ]


sizeTests : List Test
sizeTests =
    [ test "returns 0 for empty set" <|
        \() -> Expect.equal 0 (Set.size Set.empty)
    , test "returns 1 for singleton set" <|
        \() -> Expect.equal 1 (Set.size set42)
    , test "returns 100 for set of 100" <|
        \() -> Expect.equal 100 (Set.size set1To100)
    ]


foldlTests : List Test
foldlTests =
    [ test "with insert and empty set acts as identity function" <|
        \() -> Expect.equal set1To100 (Set.foldl (Set.insert compare) Set.empty set1To100)
    , test "with counter and zero acts as size function" <|
        \() -> Expect.equal 100 (Set.foldl (\_ count -> count + 1) 0 set1To100)
    , test "folds set elements from lowest to highest" <|
        \() -> Expect.equal [ 3, 2, 1 ] (Set.foldl (\n ns -> n :: ns) [] (Set.fromList compare [ 2, 1, 3 ]))
    ]


foldrTests : List Test
foldrTests =
    [ test "with insert and empty set acts as identity function" <|
        \() -> Expect.equal set1To100 (Set.foldr (Set.insert compare) Set.empty set1To100)
    , test "with counter and zero acts as size function" <|
        \() -> Expect.equal 100 (Set.foldr (\_ count -> count + 1) 0 set1To100)
    , test "folds set elements from highest to lowest" <|
        \() -> Expect.equal [ 1, 2, 3 ] (Set.foldr (\n ns -> n :: ns) [] (Set.fromList compare [ 2, 1, 3 ]))
    ]


mapTests : List Test
mapTests =
    [ test "applies given function to singleton element" <|
        \() -> Expect.equal (Set.singleton 43) (Set.map compare ((+) 1) set42)
    , test "applies given function to each element" <|
        \() -> Expect.equal (Set.fromList compare (List.range -100 -1)) (Set.map compare negate set1To100)
    ]


filterTests : List Test
filterTests =
    [ test "with always True doesn't change anything" <|
        \() -> Expect.equal set1To100 (Set.filter compare (always True) set1To100)
    , test "with always False returns empty set" <|
        \() -> Expect.equal Set.empty (Set.filter compare (always False) set1To100)
    , test "simple filter" <|
        \() -> Expect.equal set1To50 (Set.filter compare isLessThan51 set1To100)
    ]


partitionTests : List Test
partitionTests =
    [ test "of empty set returns two empty sets" <|
        \() -> Expect.equal ( Set.empty, Set.empty ) (Set.partition compare isLessThan51 Set.empty)
    , test "simple partition" <|
        \() -> Expect.equal ( set1To50, set51To100 ) (Set.partition compare isLessThan51 set1To100)
    ]


unionTests : List Test
unionTests =
    [ test "with empty set doesn't change anything" <|
        \() -> Expect.equal set42 (Set.union compare set42 Set.empty)
    , test "with itself doesn't change anything" <|
        \() -> Expect.equal set1To100 (Set.union compare set1To100 set1To100)
    , test "with subset doesn't change anything" <|
        \() -> Expect.equal set1To100 (Set.union compare set1To100 set42)
    , test "with superset returns superset" <|
        \() -> Expect.equal set1To100 (Set.union compare set42 set1To100)
    , test "contains elements of both singletons" <|
        \() -> Expect.equal (Set.insert compare 1 set42) (Set.union compare set42 (Set.singleton 1))
    , test "consists of elements from either set" <|
        \() ->
            Set.union compare set1To100 set51To150
                |> Expect.equal (Set.fromList compare (List.range 1 150))
    ]


intersectTests : List Test
intersectTests =
    [ test "with empty set returns empty set" <|
        \() -> Expect.equal Set.empty (Set.intersect compare set42 Set.empty)
    , test "with itself doesn't change anything" <|
        \() -> Expect.equal set1To100 (Set.intersect compare set1To100 set1To100)
    , test "with subset returns subset" <|
        \() -> Expect.equal set42 (Set.intersect compare set1To100 set42)
    , test "with superset doesn't change anything" <|
        \() -> Expect.equal set42 (Set.intersect compare set42 set1To100)
    , test "returns empty set given disjunctive sets" <|
        \() -> Expect.equal Set.empty (Set.intersect compare set42 (Set.singleton 1))
    , test "consists of common elements only" <|
        \() ->
            Set.intersect compare set1To100 set51To150
                |> Expect.equal set51To100
    ]


diffTests : List Test
diffTests =
    [ test "with empty set doesn't change anything" <|
        \() -> Expect.equal set42 (Set.diff compare set42 Set.empty)
    , test "with itself returns empty set" <|
        \() -> Expect.equal Set.empty (Set.diff compare set1To100 set1To100)
    , test "with subset returns set without subset elements" <|
        \() -> Expect.equal (Set.remove compare 42 set1To100) (Set.diff compare set1To100 set42)
    , test "with superset returns empty set" <|
        \() -> Expect.equal Set.empty (Set.diff compare set42 set1To100)
    , test "doesn't change anything given disjunctive sets" <|
        \() -> Expect.equal set42 (Set.diff compare set42 (Set.singleton 1))
    , test "only keeps values that don't appear in the second set" <|
        \() ->
            Set.diff compare set1To100 set51To150
                |> Expect.equal set1To50
    ]


toListTests : List Test
toListTests =
    [ test "returns empty list for empty set" <|
        \() -> Expect.equal [] (Set.toList Set.empty)
    , test "returns singleton list for singleton set" <|
        \() -> Expect.equal [ 42 ] (Set.toList set42)
    , test "returns sorted list of set elements" <|
        \() -> Expect.equal (List.range 1 100) (Set.toList set1To100)
    ]


fromListTests : List Test
fromListTests =
    [ test "returns empty set for empty list" <|
        \() -> Expect.equal Set.empty (Set.fromList compare [])
    , test "returns singleton set for singleton list" <|
        \() -> Expect.equal set42 (Set.fromList compare [ 42 ])
    , test "returns set with unique list elements" <|
        \() -> Expect.equal set1To100 (Set.fromList compare (1 :: List.range 1 100))
    ]
