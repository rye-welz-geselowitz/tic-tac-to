module Utils exposing (generateListInRange, updateListAt)


generateListInRange : Int -> (Int -> a) -> List a
generateListInRange upperBound fn =
    --TODO: better name
    List.range 0 upperBound |> List.map fn


updateListAt : Int -> (a -> a) -> List a -> List a
updateListAt targetIdx update list =
    list |> List.indexedMap (updateIfTarget targetIdx update)


updateIfTarget : Int -> (a -> a) -> Int -> a -> a
updateIfTarget targetIdx update idx item =
    if targetIdx == idx then
        update item
    else
        item
