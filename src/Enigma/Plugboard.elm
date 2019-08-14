module Enigma.Plugboard exposing (CharPosition(..), Plugboard, defaultPlugboard, pressChar, resetPlugBoard, substituteCharacter)

import List.Extra


type alias Plugboard =
    { switchedCharsList : List ( Int, Int )
    , selectedInputChar : Maybe Int
    , selectedOutputChar : Maybe Int
    }


type CharPosition
    = Input
    | Output



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Get the default plugboard with no entry's int he switchedCharList and no selectedInput/OutputChar
-}
defaultPlugboard : Plugboard
defaultPlugboard =
    { switchedCharsList = [], selectedInputChar = Nothing, selectedOutputChar = Nothing }


{-| Press a char on the plugboard. If the corresponding char is selected, the pair will be added to the switchedCharList.
plugboard - plugboard that will be used
charPosition - is the selected char an input or output value
selectedCharIndex - index of the character in the alphabet
result - the updated plugboard
-}
pressChar : Plugboard -> CharPosition -> Int -> Plugboard
pressChar plugboard charPosition selectedCharIndex =
    case charPosition of
        Input ->
            case plugboard.selectedOutputChar of
                Nothing ->
                    removeOldConnection selectedCharIndex { plugboard | selectedInputChar = Just selectedCharIndex }

                Just outputChar ->
                    addPair { plugboard | selectedInputChar = Nothing, selectedOutputChar = Nothing } ( selectedCharIndex, outputChar )

        Output ->
            case plugboard.selectedInputChar of
                Nothing ->
                    removeOldConnection selectedCharIndex { plugboard | selectedOutputChar = Just selectedCharIndex }

                Just inputChar ->
                    addPair { plugboard | selectedInputChar = Nothing, selectedOutputChar = Nothing } ( inputChar, selectedCharIndex )


{-| Substitute the given char with the plugboard
plugboard - that should be used
charIndex - the index of the character
result - the substituted char
-}
substituteCharacter : Int -> Plugboard -> Int
substituteCharacter charIndex plugboard =
    let
        maybeEntry =
            List.Extra.find (\( inputParam, outputParam ) -> charIndex == inputParam || charIndex == outputParam) plugboard.switchedCharsList

        -- Default is same input and output
        ( input, output ) =
            Maybe.withDefault ( charIndex, charIndex ) maybeEntry
    in
    if charIndex == input then
        output

    else
        input


{-| reset the plugboard. Remove all connection and set the selected Input/OutputChar to Nothing
-}
resetPlugBoard : Plugboard -> Plugboard
resetPlugBoard plugboard =
    { plugboard | switchedCharsList = [], selectedInputChar = Nothing, selectedOutputChar = Nothing }



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| add the given Pair of input and output char to the switchedCharsList.
-}
addPair : Plugboard -> ( Int, Int ) -> Plugboard
addPair plugboard ( input, output ) =
    let
        updatedPlugboard =
            plugboard |> removeOldConnection input |> removeOldConnection output

        newSwitchedCharsList =
            ( input, output ) :: plugboard.switchedCharsList
    in
    { updatedPlugboard | switchedCharsList = newSwitchedCharsList }


{-| Remove a old connection with the given char when existing
-}
removeOldConnection : Int -> Plugboard -> Plugboard
removeOldConnection charIndex plugboard =
    let
        filteredList =
            List.filter (\( input, output ) -> not (input == charIndex || output == charIndex)) plugboard.switchedCharsList
    in
    { plugboard | switchedCharsList = filteredList }
