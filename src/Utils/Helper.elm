module Utils.Helper exposing (foldl2, foldr2, testFunction, testFunctionHelper)

import Enigma.Rotor exposing (..)


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


foldl2Helper : (a -> b -> b -> a) -> a -> List b -> b -> b -> a
foldl2Helper f input list neutralElement previousElement =
    case list of
        [] ->
            f input neutralElement previousElement

        x :: xs ->
            foldl2Helper f (f input x previousElement) xs neutralElement x


{-| Reduce the given list from the right and perform the given method on th element a.
In the given function, the first element b is the current element in the list. The second element
in the list is the previous one.
The neutral element will be required at the beginning and at the end of the list. So the first call will
be (a -> currentElement -> neutralElement -> a) and the last element call will
be (a -> neutralElement -> previousElement -> a)
-}
foldr2 : (a -> b -> b -> a) -> a -> List b -> b -> a
foldr2 f input list neutralElement =
    foldl2 f input (List.reverse list) neutralElement


testFunction : Char -> Maybe Char
testFunction input =
    let
        rotor3Test =
            { rotor3 | currentPosition = 1 }

        rotor2Test =
            { rotor2 | currentPosition = 3 }

        rotor1Test =
            { rotor1 | currentPosition = 5 }

        rotorList =
            [ rotor3Test, rotor2Test, rotor1Test, staticRotor ]
    in
    foldl2 testFunctionHelper (Just 'A') rotorList staticRotor


testFunctionHelper : Maybe Char -> Rotor -> Rotor -> Maybe Char
testFunctionHelper inputChar currentRotor prevRotor =
    case inputChar of
        Nothing ->
            Nothing

        Just a ->
            substituteCharacter (Debug.log "test " a) currentRotor prevRotor ToReflector
