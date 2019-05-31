module Utils.AlphabetHelper exposing (alphabetSequence, increaseAlphabetCharacter, indexOfCharacterInSequence, isLetterInAlphabet)

import Array exposing (Array)


alphabetSequence =
    Array.fromList (String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ")


{-| Return true when the character is in the alphabet.
Function is true, when the character is either between a-z or A-Z
-}
isLetterInAlphabet : Char -> Bool
isLetterInAlphabet inputChar =
    let
        upperChar =
            Char.toUpper inputChar
    in
    'A' <= upperChar && upperChar <= 'Z'


{-| get the index of the given Char in the given Char array.
If the Char does not exist the result is Nothing
-}
indexOfCharacterInSequence : Char -> Array Char -> Maybe Int
indexOfCharacterInSequence inputChar charSequence =
    indexOfCharacterInSequenceHelper inputChar charSequence 0


{-| Helper function the get the index of the given Char in the Char array.
The given index must be initial 0
-}
indexOfCharacterInSequenceHelper : Char -> Array Char -> Int -> Maybe Int
indexOfCharacterInSequenceHelper inputChar charSequence currentIndex =
    case Array.get currentIndex charSequence of
        Just currentChar ->
            if Char.toUpper currentChar == Char.toUpper inputChar then
                Just currentIndex

            else
                indexOfCharacterInSequenceHelper inputChar charSequence (currentIndex + 1)

        Nothing ->
            Nothing


{-| Increase the given character of the alphabet by the given number.
The function will return only capital characters. The result is nothing when the given character
is not in the alphabet.
-}
increaseAlphabetCharacter : Char -> Int -> Maybe Char
increaseAlphabetCharacter inputChar amount =
    let
        codeOfA =
            Char.toCode 'A'

        inputCharCode =
            Char.toCode (Char.toUpper inputChar)

        indexOfCharInAlphabet =
            modBy 26 (inputCharCode - codeOfA + amount)
    in
    if isLetterInAlphabet inputChar then
        Just (Char.fromCode (codeOfA + indexOfCharInAlphabet))

    else
        Nothing
