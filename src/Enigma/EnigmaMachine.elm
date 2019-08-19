module Enigma.EnigmaMachine exposing
    ( Enigma
    , RandomizationType(..)
    , defaultEnigma
    , getRandomCharPositionsCmd
    , handleCmdResult
    , performRotationAndSubstitution
    , performRotationsAndSubstitutions
    , pressCharOnPlugBoard
    , randomizeReflectorCmd
    , randomizeRotorsCmd
    , replaceReflector
    , replaceRotor
    , resetPlugBoard
    , setCurrentPositionToStartPosition
    , setRingPositionOfRotor
    , setStartPositionOfRotor
    , substituteCharacter
    )

import Dict
import Enigma.Plugboard exposing (Plugboard)
import Enigma.Reflector exposing (Reflector)
import Enigma.Rotor exposing (Rotor, rotor1, rotor2, rotor3, staticRotor)
import List
import List.Extra
import Random
import Random.List
import Utils.AlphabetHelper
import Utils.Helper


type alias Enigma =
    { rotors : List Rotor
    , reflector : Reflector
    , plugBoard : Plugboard
    }


defaultEnigma : Enigma
defaultEnigma =
    let
        rotor1Test =
            { rotor1 | currentPosition = 0, startPosition = 0, ringPosition = 0 }

        rotor2Test =
            { rotor2 | currentPosition = 0, startPosition = 0, ringPosition = 0 }

        rotor3Test =
            { rotor3 | currentPosition = 0, startPosition = 0, ringPosition = 0 }

        rotorList =
            [ rotor1Test, rotor2Test, rotor3Test ]
    in
    { rotors = rotorList, reflector = Enigma.Reflector.reflectorB, plugBoard = Enigma.Plugboard.defaultPlugboard }


type RandomizationType
    = RandomizePlugboard (List Int)
    | RandomizeRotor ( Int, Rotor )
    | RandomizeReflector Reflector
    | RandomizeRotorStartPosition ( Int, Int )
    | RandomizeRotorRingPosition ( Int, Int )



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
        |> Debug.log "SubstituteCharacterWithPlugboard" (substituteCharacterWithPlugboard enigma)
        |> Debug.log "Calling substituteCharacterToReflector" (substituteCharacterToReflector enigma)
        |> Debug.log "Calling substituteCharacterWithReflector" (substituteCharacterWithReflector enigma)
        |> Debug.log "Calling substituteCharacterFromReflector" (substituteCharacterFromReflector enigma)
        |> Debug.log "SubstituteCharacterWithPlugboard" (substituteCharacterWithPlugboard enigma)
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


{-| Replace the reflector of the enigma
-}
replaceReflector : Enigma -> Reflector -> Enigma
replaceReflector enigma newReflector =
    { enigma | reflector = newReflector }


{-| Set the StartPosition of the rotor at the given rotorIndex to the given startPosition
-}
setStartPositionOfRotor : Enigma -> Int -> Int -> Enigma
setStartPositionOfRotor enigma rotorIndex newStartPosition =
    let
        newRotors =
            --            TODO Remove currentPosition
            List.Extra.updateAt rotorIndex (\rotor -> { rotor | startPosition = newStartPosition, currentPosition = newStartPosition }) enigma.rotors
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


{-| Press a char on the plugboard.
enigma - the enigma with the plugboard
charPosition - is the selected char an input or output value
charIndex - index of the character in the alphabet
-}
pressCharOnPlugBoard : Enigma -> Enigma.Plugboard.CharPosition -> Int -> Enigma
pressCharOnPlugBoard enigma charPosition charIndex =
    { enigma | plugBoard = Enigma.Plugboard.pressChar enigma.plugBoard charPosition charIndex }


{-| reset the plugboard. Remove all stored connections and set the selected Input/OutputChar to nothing
-}
resetPlugBoard : Enigma -> Enigma
resetPlugBoard enigma =
    { enigma | plugBoard = Enigma.Plugboard.resetPlugBoard enigma.plugBoard }


{-| Set the currentPosition in each rotor to the selected startPosition
-}
setCurrentPositionToStartPosition : Enigma -> Enigma
setCurrentPositionToStartPosition enigma =
    let
        updatedRotors =
            List.map (\rotor -> { rotor | currentPosition = rotor.startPosition }) enigma.rotors
    in
    { enigma | rotors = updatedRotors }


{-| Create a Cmd to randomize all rotors in the given enigma
-}
randomizeRotorsCmd : (( Int, Rotor ) -> msg) -> Enigma -> Cmd msg
randomizeRotorsCmd function enigma =
    Cmd.batch (List.indexedMap (\index _ -> randomizeRotor function index) enigma.rotors)


{-| Create a command to select a random reflector
-}
randomizeReflectorCmd : (Reflector -> msg) -> Cmd msg
randomizeReflectorCmd function =
    Dict.values Enigma.Reflector.getAllReflectors
        |> Random.List.choose
        |> Random.map (Maybe.withDefault Enigma.Reflector.reflectorB << Tuple.first)
        |> Random.generate function


{-| Create a command to select a random character for each rotor in the given enigma
-}
getRandomCharPositionsCmd : (( Int, Int ) -> msg) -> Enigma -> Cmd msg
getRandomCharPositionsCmd function enigma =
    Cmd.batch (List.indexedMap (\index _ -> randomCharPosition function index) enigma.rotors)


{-| Handle the result of randomizing a component of the enigma.
enigma - the enigma that will be modified
randomizationType - the part of the enigma that will be randomized
-}
handleCmdResult : Enigma -> RandomizationType -> Enigma
handleCmdResult enigma randomizationType =
    case randomizationType of
        RandomizePlugboard newPlugboardList ->
            { enigma | plugBoard = Enigma.Plugboard.handleRandomPlugboardCmd enigma.plugBoard newPlugboardList }

        RandomizeRotor ( rotorPosition, newRotor ) ->
            replaceRotor enigma rotorPosition newRotor

        RandomizeReflector newReflector ->
            replaceReflector enigma newReflector

        RandomizeRotorStartPosition ( rotorPosition, newStartPosition ) ->
            setStartPositionOfRotor enigma rotorPosition newStartPosition

        RandomizeRotorRingPosition ( rotorPosition, newRingPosition ) ->
            setRingPositionOfRotor enigma rotorPosition newRingPosition



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


substituteCharacterWithPlugboard : Enigma -> Maybe Int -> Maybe Int
substituteCharacterWithPlugboard enigma maybeCharIndex =
    case maybeCharIndex of
        Nothing ->
            Nothing

        Just charIndex ->
            Just (Enigma.Plugboard.substituteCharacter charIndex enigma.plugBoard)


{-| Replace the character with the rotors of the enigma in the direction to the reflector
-}
substituteCharacterToReflector : Enigma -> Maybe Int -> Maybe Int
substituteCharacterToReflector enigma maybeInputIndex =
    Utils.Helper.foldr2
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
    Utils.Helper.foldl2
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
        ( newRotorList, _ ) =
            List.foldr
                (\rotor ( rotorList, shouldRotateRotor ) ->
                    let
                        ( updatedRotor, shouldRotateNext ) =
                            rotateRotor rotor shouldRotateRotor
                    in
                    ( updatedRotor :: rotorList, shouldRotateNext )
                )
                ( [], True )
                enigma.rotors
    in
    { enigma | rotors = newRotorList }


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


{-| Randomize the rotor for the given index
function - parse the index and the rotor to a msg
rotorIndex - the index of the selected rotor
-}
randomizeRotor : (( Int, Rotor ) -> msg) -> Int -> Cmd msg
randomizeRotor function rotorIndex =
    Random.List.choose (Dict.values Enigma.Rotor.getAllRotors)
        |> Random.map
            (\( maybeRandomRotor, _ ) ->
                ( rotorIndex, Maybe.withDefault rotor1 maybeRandomRotor )
            )
        |> Random.generate function


{-| Get a command to receive a random character for the given index
function - to convert the pair of position and random character in a message
charIndex - position of the character
-}
randomCharPosition : (( Int, Int ) -> msg) -> Int -> Cmd msg
randomCharPosition function position =
    Random.int 0 25
        |> Random.map (\randomChar -> ( position, randomChar ))
        |> Random.generate function
