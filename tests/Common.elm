module Common exposing (expectEqual)

import DictAny as Dict exposing (Dict)
import Expect exposing (Expectation)


expectEqual : Dict k v -> Dict k v -> Expectation
expectEqual expected actual =
    actual
        |> Dict.toList
        |> Expect.equalLists (Dict.toList expected)
