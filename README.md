# DictAny SetAny

Modified versions of Dict and Set from `elm/core`. The keys can be any (non-function) type, a comparison function is used for every action that needs to compare keys. The comparison function is `k -> k -> Order` so the signatures can be `DictAny k v` and `SetAny a`.

## Usage

For `SetAny a` the comparison function `a -> a -> Order` is needed.

    import SetAny as Set exposing (Set)

    items : Set Item
    items = [ Brush, Soap, Towel ] |> Set.fromList comparer

    type Item
        = Brush
        | Soap
        | Shampoo
        | Towel

    itemToInt : Item -> Int
    itemToInt Item =
        case Item of
            Brush -> 1
            Soap -> 2
            Shampoo -> 3
            Towel -> 42

    comparer : Item -> Item -> Order
    comparer a b = compare (itemToInt a) (itemToInt b)

    items |> Set.member comparer Shampoo --> False
    items |> Set.member comparer Towel  --> True
    items |> Set.toList --> [Towel, Soap, Brush]

## Design Goals

No functions stored in the model. Some other any key type implementations use a `k -> comparable` function, which requires the type signatures to include the `comparable` type, this is not necessary with this package.

## Performance

The code is cloned from `elm/core` and the ordering function added. Performance depends on the complexity of the comparison function, but otherwise the same as the `elm/core` versions.

## Downsides

The correct comparison function must be passed in every time, no compile time checking for this. If the Dict and Set code in `elm/core` is updated in the future, this package will also need to be updated to implement the changes.

## Implementation notes

I did try changing the internal names `RBNode_elm_builtin` and `RBEmpty_elm_builtin`, but the tests then failed. I found that these are magic values in the elm compiler, see here:
[github.com/elm/compiler](https://github.com/search?q=repo%3Aelm%2Fcompiler+++RBEmpty_elm_builtin&type=code).
