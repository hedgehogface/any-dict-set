module SetAny exposing
    ( Set
    , empty, singleton, insert, remove
    , isEmpty, member, size
    , union, intersect, diff
    , toList, fromList
    , map, foldl, foldr, filter, partition
    )

{-| A set of values unique according to a comparison function. Values can be any type.

A comparison function `a -> a -> Order` is used to compare values, this should uniquely identify values.

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

    import SetAny as Set exposing (Set)

    items : Set Item
    items = [ Brush, Soap, Towel ] |> Set.fromList comparer

    type Item
        = Brush
        | Soap
        | Shampoo
        | Towel

    itemToInt : Item -> Int
    itemToInt item =
        case item of
            Brush -> 1
            Soap -> 2
            Shampoo -> 3
            Towel -> 42

    comparer : Item -> Item -> Order
    comparer a b = compare (itemToInt a) (itemToInt b)

    items |> Set.member comparer Shampoo --> False
    items |> Set.member comparer Towel  --> True
    items |> Set.toList --> [ Brush, Soap, Towel ]

-}
type Set t
    = Set_elm_builtin (DictAny.Dict t ())


{-| Create an empty set.
-}
empty : Set a
empty =
    Set_elm_builtin DictAny.empty


{-| Create a set with one value.
-}
singleton : a -> Set a
singleton key =
    Set_elm_builtin (DictAny.singleton key ())


{-| Insert a value into a set.
-}
insert : (a -> a -> Order) -> a -> Set a -> Set a
insert orderer key (Set_elm_builtin dict) =
    Set_elm_builtin (DictAny.insert orderer key () dict)


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : (a -> a -> Order) -> a -> Set a -> Set a
remove orderer key (Set_elm_builtin dict) =
    Set_elm_builtin (DictAny.remove orderer key dict)


{-| Determine if a set is empty.
-}
isEmpty : Set a -> Bool
isEmpty (Set_elm_builtin dict) =
    DictAny.isEmpty dict


{-| Determine if a value is in a set.
-}
member : (a -> a -> Order) -> a -> Set a -> Bool
member orderer key (Set_elm_builtin dict) =
    DictAny.member orderer key dict


{-| Determine the number of elements in a set.
-}
size : Set a -> Int
size (Set_elm_builtin dict) =
    DictAny.size dict


{-| Get the union of two sets. Keep all values.
-}
union : (a -> a -> Order) -> Set a -> Set a -> Set a
union orderer (Set_elm_builtin dict1) (Set_elm_builtin dict2) =
    Set_elm_builtin (DictAny.union orderer dict1 dict2)


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : (a -> a -> Order) -> Set a -> Set a -> Set a
intersect orderer (Set_elm_builtin dict1) (Set_elm_builtin dict2) =
    Set_elm_builtin (DictAny.intersect orderer dict1 dict2)


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : (a -> a -> Order) -> Set a -> Set a -> Set a
diff orderer (Set_elm_builtin dict1) (Set_elm_builtin dict2) =
    Set_elm_builtin (DictAny.diff orderer dict1 dict2)


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : Set a -> List a
toList (Set_elm_builtin dict) =
    DictAny.keys dict


{-| Convert a list into a set, removing any duplicates.
-}
fromList : (a -> a -> Order) -> List a -> Set a
fromList orderer list =
    List.foldl (insert orderer) empty list


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> Set a -> b
foldl func initialState (Set_elm_builtin dict) =
    DictAny.foldl (\key _ state -> func key state) initialState dict


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> Set a -> b
foldr func initialState (Set_elm_builtin dict) =
    DictAny.foldr (\key _ state -> func key state) initialState dict


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (b -> b -> Order) -> (a -> b) -> Set a -> Set b
map ordererb func set =
    fromList ordererb (foldl (\x xs -> func x :: xs) [] set)


{-| Only keep elements that pass the given test.

    import SetAny as Set exposing (Set)

    numbers : Set Int
    numbers =
        Set.fromList compare [ -2, -1, 0, 1, 2 ]

    positives : Set Int
    positives =
        Set.filter compare (\x -> x > 0) numbers

    --> positives == Set.fromList compare [1,2]

-}
filter : (a -> a -> Order) -> (a -> Bool) -> Set a -> Set a
filter orderer isGood (Set_elm_builtin dict) =
    Set_elm_builtin (DictAny.filter orderer (\key _ -> isGood key) dict)


{-| Create two new sets. The first contains all the elements that passed the
given test, and the second contains all the elements that did not.
-}
partition : (a -> a -> Order) -> (a -> Bool) -> Set a -> ( Set a, Set a )
partition orderer isGood (Set_elm_builtin dict) =
    let
        ( dict1, dict2 ) =
            DictAny.partition orderer (\key _ -> isGood key) dict
    in
    ( Set_elm_builtin dict1, Set_elm_builtin dict2 )
