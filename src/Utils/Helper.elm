module Utils.Helper exposing (foldl2, foldr2, map2)

{-| -}

-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Reduce the given list from the left and perform the given method on th element a.
In the given function, the first element b is the current element in the list. The second element
in the list is the previous one.
The neutral element will be required at the beginning and at the end of the list. So the first call will
be (a -> currentElement -> neutralElement -> a) and the last element call will
be (a -> neutralElement -> previousElement -> a)
-}
foldl2 : (a -> b -> b -> a) -> a -> List b -> b -> a
foldl2 f input list neutralElement =
    foldl2Helper f input list neutralElement neutralElement


{-| Reduce the given list from the right and perform the given method on th element a.
In the given function, the first element b is the current element in the list. The second element
in the list is the previous one.
The neutral element will be required at the beginning and at the end of the list. So the first call will
be (a -> currentElement -> neutralElement -> a) and the last element call will
be (a -> neutralElement -> previousElement -> a)
-}
foldr2 : (a -> b -> b -> a) -> a -> List b -> b -> a
foldr2 f input list =
    foldl2 f input (List.reverse list)


{-| Map the elements in the given list with the function and return the result.
The function offers an extra element a. The initial value will be passed to the first call of the function f. After that
the value a in the result will always be passed to the next element
THe function offers in f the the current, and the next element. If the next element is not existing
(at the end of the list) the param will be Nothing.
-}
map2 : List b -> (b -> Maybe b -> a -> ( a, c )) -> a -> List c
map2 list =
    map2Helper (List.reverse list) []



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


foldl2Helper : (a -> b -> b -> a) -> a -> List b -> b -> b -> a
foldl2Helper f input list currentElement previousElement =
    case list of
        [] ->
            f input currentElement previousElement

        x :: xs ->
            foldl2Helper f (f input x previousElement) xs currentElement x


map2Helper : List b -> List c -> (b -> Maybe b -> a -> ( a, c )) -> a -> List c
map2Helper list resultList f extra =
    case list of
        [] ->
            resultList

        x :: xs ->
            let
                nextElement =
                    List.head xs

                ( nextExtra, resultElement ) =
                    f x nextElement extra
            in
            map2Helper xs (resultElement :: resultList) f nextExtra
