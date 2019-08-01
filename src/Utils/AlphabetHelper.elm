module Utils.AlphabetHelper exposing (characterIndexToCharacter, characterToCharacterIndex, isLetterInAlphabet)

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
