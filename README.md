# DictAny SetAny

Modified versions of Dict and Set from `elm/core`. The keys can be any type but a comparison function has to be used for every action that needs to compare keys. The comparison funtcion is `k -> k -> Order`, so the signatures can be `DictAny k v` and `SetAny a`.

## Usage

For `SetAny a` a comparison function `a -> a -> Order` is needed.

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

No functions stored in the model. Some other any key implementations use a `k -> comparable` function, which means the type signatures have to include the `comparable` type, this is not necessary with this package.

## Performance

The code is cloned from `elm/core` and the ordering function added. Performance depends on the complexity of the comparison function, but otherwise the same as the `elm/core` versions.

## Downsides

The correct comparison function must be passed in every time, no compile time checking for this.

