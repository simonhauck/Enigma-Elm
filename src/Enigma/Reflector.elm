module Enigma.Reflector exposing (Reflector, getAllReflectors, reflectorA, reflectorB, reflectorC, subsituteCharacter)

import Array exposing (Array)
import Dict exposing (Dict)
import Utils.AlphabetHelper


{-| -}
type alias Reflector =
    Array Char


{-| Substitute the given char with the given reflector.
If the character is not in the alphabet, this function will return Nothing.
If the character is valid, then the corresponding char on the reflector will be returned
-}
subsituteCharacter : Char -> Reflector -> Maybe Char
subsituteCharacter inputChar reflector =
    case Utils.AlphabetHelper.indexOfCharacterInSequence inputChar Utils.AlphabetHelper.alphabetSequence of
        Just index ->
            Array.get index reflector

        Nothing ->
            Nothing


{-| Get a Dic with all available reflectors.
The key is the name of the reflector.
The value is the reflector with the defined fields.
-}
getAllReflectors : Dict String Reflector
getAllReflectors =
    Dict.fromList
        [ ( "Reflector A", reflectorA )
        , ( "Reflector B", reflectorB )
        , ( "Reflector C", reflectorC )
        ]


reflectorA =
    Array.fromList (String.toList "EJMZALYXVBWFCRQUONTSPIKHGD")


reflectorB =
    Array.fromList (String.toList "YRUHQSLDPXNGOKMIEBFZCWVJAT")


reflectorC =
    Array.fromList (String.toList "FVPJIAOYEDRZXWGCTKUQSBNMHL")
