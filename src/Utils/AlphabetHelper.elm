module Utils.AlphabetHelper exposing (characterIndexToCharacter, characterToCharacterIndex, isLetterInAlphabet)

{-| -}


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


{-| Parse a int value into a char.
The int must be between 0 and 25. If the Int val is Nothing or not in this range the result is Nothing
-}
characterIndexToCharacter : Maybe Int -> Maybe Char
characterIndexToCharacter maybeIndex =
    let
        index =
            Maybe.withDefault -1 maybeIndex
    in
    if 0 <= index && index <= 25 then
        Just (Char.fromCode (Char.toCode 'A' + index))

    else
        Nothing


{-| Get the index of the character in the alphabet.
If the char is not in the alphabet, nothing will be returned
-}
characterToCharacterIndex : Char -> Maybe Int
characterToCharacterIndex inputChar =
    let
        upperChar =
            Char.toUpper inputChar
    in
    if isLetterInAlphabet upperChar then
        Just (Char.toCode upperChar - Char.toCode 'A')

    else
        Nothing
