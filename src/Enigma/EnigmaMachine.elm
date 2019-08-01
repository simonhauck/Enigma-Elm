module Enigma.EnigmaMachine exposing (Enigma, debugEnigma, performRotationAndSubstitution, performRotationsAndSubstitutions, replaceRotor, setRingPositionOfRotor, setStartPositionOfRotor, substituteCharacter)

import Enigma.Reflector exposing (Reflector)
import Enigma.Rotor exposing (Rotor, rotor1, rotor2, rotor3, staticRotor)
import List
import List.Extra
import Utils.AlphabetHelper
import Utils.Helper


type alias Enigma =
    { rotors : List Rotor
    , reflector : Reflector
    }


debugEnigma : Enigma
debugEnigma =
    let
        rotor3Test =
            { rotor3 | currentPosition = 0, startPosition = 2, ringPosition = 1 }

        rotor2Test =
            { rotor2 | currentPosition = 0, startPosition = 5, ringPosition = 2 }

        rotor1Test =
            { rotor1 | currentPosition = 0, startPosition = 10, ringPosition = 0 }

        rotorList =
            [ rotor3Test, rotor2Test, rotor1Test ]
    in
    { rotors = rotorList, reflector = Enigma.Reflector.reflectorB }



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Rotate the rotors of the enigma and perform the substitution.
If the given Character is not in the alphabet, the result will be nothing and the rotors will not be rotated
-}
performRotationAndSubstitution : Enigma -> Char -> ( Enigma, Maybe Char )
performRotationAndSubstitution enigma input =
    if Utils.AlphabetHelper.isLetterInAlphabet input then
        let
            rotatedEnigma =
                rotateRotors enigma

            substitutionResult =
                substituteCharacter input rotatedEnigma
        in
        ( rotatedEnigma, substitutionResult )

    else
        ( enigma, Nothing )


{-| Perform the substitution for the given list of Characters.
For every Character the rotors will be rotated and the substitution performed
-}
performRotationsAndSubstitutions : Enigma -> List Char -> ( Enigma, List (Maybe Char) )
performRotationsAndSubstitutions inputEnigma inputChars =
    let
        ( outputEnigma, resultList ) =
            List.foldl
                (\input ( enigma, outCharsList ) ->
                    let
                        ( rotatedEnigma, substitutionResult ) =
                            performRotationAndSubstitution enigma input
                    in
                    ( rotatedEnigma, substitutionResult :: outCharsList )
                )
                ( inputEnigma, [] )
                inputChars
    in
    ( outputEnigma, List.reverse resultList )


{-| Substitute a character with the current enigma configuration.
The will character be replaced with the rotors to the reflector, then with the reflector, and backwards
-}
substituteCharacter : Char -> Enigma -> Maybe Char
substituteCharacter inputChar enigma =
    --    TODO remove log statements
    inputChar
        |> Debug.log "Convert char to index: " Utils.AlphabetHelper.characterToCharacterIndex
        |> Debug.log "Calling substituteCharacterToReflector" (substituteCharacterToReflector enigma)
        |> Debug.log "Calling substituteCharacterWithReflector" (substituteCharacterWithReflector enigma)
        |> Debug.log "Calling substituteCharacterFromReflector" (substituteCharacterFromReflector enigma)
        |> Debug.log "Result of substitution: " Utils.AlphabetHelper.characterIndexToCharacter


{-| Replace the rotor in the enigma at the given index with the given rotor. The position values will be
copied to new new rotor
-}
replaceRotor : Enigma -> Int -> Rotor -> Enigma
replaceRotor enigma rotorPosition newRotor =
    let
        newRotorList =
            List.Extra.updateAt rotorPosition
                (\oldRotor ->
                    { newRotor
                        | startPosition = oldRotor.startPosition
                        , ringPosition = oldRotor.ringPosition
                        , currentPosition = oldRotor.currentPosition
                    }
                )
                enigma.rotors
    in
    { enigma | rotors = newRotorList }


{-| Set the StartPosition of the rotor at the given rotorIndex to the given startPosition
-}
setStartPositionOfRotor : Enigma -> Int -> Int -> Enigma
setStartPositionOfRotor enigma rotorIndex newStartPosition =
    let
        newRotors =
            List.Extra.updateAt rotorIndex (\rotor -> { rotor | startPosition = newStartPosition }) enigma.rotors
    in
    { enigma | rotors = newRotors }


{-| Set the RingPosition of the rotor at the given rotorIndex to the given new ring position
-}
setRingPositionOfRotor : Enigma -> Int -> Int -> Enigma
setRingPositionOfRotor enigma rotorIndex newRingPosition =
    let
        newRotors =
            List.Extra.updateAt rotorIndex (\rotor -> { rotor | ringPosition = newRingPosition }) enigma.rotors
    in
    { enigma | rotors = newRotors }



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Replace the character with the rotors of the enigma in the direction to the reflector
-}
substituteCharacterToReflector : Enigma -> Maybe Int -> Maybe Int
substituteCharacterToReflector enigma maybeInputIndex =
    Utils.Helper.foldl2
        --    TODO Eta reduction
        (\input currentRotor previousRotor ->
            case input of
                Nothing ->
                    Nothing

                Just inputIndex ->
                    Debug.log "Result of CharSubstitution ToReflector: " (Enigma.Rotor.substituteCharacter Enigma.Rotor.ToReflector (Debug.log "InputChar: " inputIndex) currentRotor previousRotor)
        )
        maybeInputIndex
        enigma.rotors
        staticRotor


{-| Replace the character with the rotors of the enigma in the direction from the reflector
-}
substituteCharacterFromReflector : Enigma -> Maybe Int -> Maybe Int
substituteCharacterFromReflector enigma maybeInputIndex =
    Utils.Helper.foldr2
        --    TODO Eta reduction
        (\input currentRotor previousRotor ->
            case input of
                Nothing ->
                    Nothing

                Just inputChar ->
                    Debug.log "Result of CharSubstitution FromReflector" (Enigma.Rotor.substituteCharacter Enigma.Rotor.FromReflector (Debug.log "InputChar" inputChar) currentRotor previousRotor)
        )
        maybeInputIndex
        enigma.rotors
        staticRotor


{-| Replace the character with the reflector of the enigma
-}
substituteCharacterWithReflector : Enigma -> Maybe Int -> Maybe Int
substituteCharacterWithReflector enigma maybeInput =
    case maybeInput of
        Nothing ->
            Nothing

        Just inputChar ->
            Debug.log "Result of CharSubstitution Reflector" (Enigma.Reflector.substituteCharacter (Debug.log "InputChar" inputChar) enigma.reflector)


{-| Rotate the rotors in the enigma.
The result is the enigma with updated rotors
-}
rotateRotors : Enigma -> Enigma
rotateRotors enigma =
    let
        updatedRotors =
            rotateRotorsHelper enigma.rotors True []
    in
    { enigma | rotors = updatedRotors }


{-| Helper Method for rotateRotors.
Rotate the rotors in the given list and return the result
-}
rotateRotorsHelper : List Rotor -> Bool -> List Rotor -> List Rotor
rotateRotorsHelper rotors shouldRotateRotor accumulator =
    case rotors of
        [] ->
            List.reverse accumulator

        x :: xs ->
            let
                ( rotor, shouldRotateNext ) =
                    rotateRotor x shouldRotateRotor

                newAccumulator =
                    rotor :: accumulator
            in
            rotateRotorsHelper xs shouldRotateNext newAccumulator


{-| Rotate a single rotor.
rotor - The rotor that should be rotated
shouldRotateRotor - The rotor will only be rotated if this value is true.
result - The rotor and a boolean value, if the next rotor should be rotated
-}
rotateRotor : Rotor -> Bool -> ( Rotor, Bool )
rotateRotor rotor shouldRotateRotor =
    if shouldRotateRotor then
        let
            shouldRotateNextRotor =
                List.member rotor.currentPosition rotor.turningPoints

            rotatedRotor =
                Enigma.Rotor.rotateRotor rotor
        in
        ( rotatedRotor, shouldRotateNextRotor )

    else
        ( rotor, False )
