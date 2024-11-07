module SetAny exposing
    ( Set
    , empty, singleton, insert, remove
    , isEmpty, member, size
    , union, intersect, diff
    , toList, fromList
    , map, foldl, foldr, filter, partition
    )

{-| A set of unique values. The values can be any comparable type. This
includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists
of comparable types.

Insert, remove, and query operations all take _O(log n)_ time.


# Sets

@docs Set


# Build

@docs empty, singleton, insert, remove


# Query

@docs isEmpty, member, size


# Combine

@docs union, intersect, diff


# Lists

@docs toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition

-}

import Basics exposing (Bool, Int)
import DictAny
import List exposing ((::))
import Maybe exposing (Maybe(..))


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type Set t
    = Set_elm_builtin_a (DictAny.Dict t ())


{-| Create an empty set.
-}
empty : Set a
empty =
    Set_elm_builtin_a DictAny.empty


{-| Create a set with one value.
-}
singleton : a -> Set a
singleton key =
    Set_elm_builtin_a (DictAny.singleton key ())


{-| Insert a value into a set.
-}
insert : (a -> a -> Order) -> a -> Set a -> Set a
insert orderer key (Set_elm_builtin_a dict) =
    Set_elm_builtin_a (DictAny.insert orderer key () dict)


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : (a -> a -> Order) -> a -> Set a -> Set a
remove orderer key (Set_elm_builtin_a dict) =
    Set_elm_builtin_a (DictAny.remove orderer key dict)


{-| Determine if a set is empty.
-}
isEmpty : Set a -> Bool
isEmpty (Set_elm_builtin_a dict) =
    DictAny.isEmpty dict


{-| Determine if a value is in a set.
-}
member : (a -> a -> Order) -> a -> Set a -> Bool
member orderer key (Set_elm_builtin_a dict) =
    DictAny.member orderer key dict


{-| Determine the number of elements in a set.
-}
size : Set a -> Int
size (Set_elm_builtin_a dict) =
    DictAny.size dict


{-| Get the union of two sets. Keep all values.
-}
union : (a -> a -> Order) -> Set a -> Set a -> Set a
union orderer (Set_elm_builtin_a dict1) (Set_elm_builtin_a dict2) =
    Set_elm_builtin_a (DictAny.union orderer dict1 dict2)


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : (a -> a -> Order) -> Set a -> Set a -> Set a
intersect orderer (Set_elm_builtin_a dict1) (Set_elm_builtin_a dict2) =
    Set_elm_builtin_a (DictAny.intersect orderer dict1 dict2)


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : (a -> a -> Order) -> Set a -> Set a -> Set a
diff orderer (Set_elm_builtin_a dict1) (Set_elm_builtin_a dict2) =
    Set_elm_builtin_a (DictAny.diff orderer dict1 dict2)


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : Set a -> List a
toList (Set_elm_builtin_a dict) =
    DictAny.keys dict


{-| Convert a list into a set, removing any duplicates.
-}
fromList : (a -> a -> Order) -> List a -> Set a
fromList orderer list =
    List.foldl (insert orderer) empty list


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> Set a -> b
foldl func initialState (Set_elm_builtin_a dict) =
    DictAny.foldl (\key _ state -> func key state) initialState dict


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> Set a -> b
foldr func initialState (Set_elm_builtin_a dict) =
    DictAny.foldr (\key _ state -> func key state) initialState dict


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (b -> b -> Order) -> (a -> b) -> Set a -> Set b
map ordererb func set =
    fromList ordererb (foldl (\x xs -> func x :: xs) [] set)


{-| Only keep elements that pass the given test.

    import Set exposing (Set)

    numbers : Set Int
    numbers =
        Set.fromList [ -2, -1, 0, 1, 2 ]

    positives : Set Int
    positives =
        Set.filter (\x -> x > 0) numbers

    -- positives == Set.fromList [1,2]

-}
filter : (a -> a -> Order) -> (a -> Bool) -> Set a -> Set a
filter orderer isGood (Set_elm_builtin_a dict) =
    Set_elm_builtin_a (DictAny.filter orderer (\key _ -> isGood key) dict)


{-| Create two new sets. The first contains all the elements that passed the
given test, and the second contains all the elements that did not.
-}
partition : (a -> a -> Order) -> (a -> Bool) -> Set a -> ( Set a, Set a )
partition orderer isGood (Set_elm_builtin_a dict) =
    let
        ( dict1, dict2 ) =
            DictAny.partition orderer (\key _ -> isGood key) dict
    in
    ( Set_elm_builtin_a dict1, Set_elm_builtin_a dict2 )
