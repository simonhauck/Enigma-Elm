module Models.Enigma.EnigmaMachine exposing
    ( Enigma
    , RandomizationType(..)
    , defaultEnigma
    , getRandomCharPositionsCmd
    , handleRandomizeResult
    , performRotationAndSubstitution
    , pressCharOnPlugBoard
    , randomizeReflectorCmd
    , randomizeRotorsCmd
    , resetPlugBoard
    , setReflector
    , setRingPositionOfRotor
    , setRotor
    , setStartPositionAsCurrentPosition
    , setStartPositionOfRotor
    , substituteCharacter
    )

import Dict
import List
import List.Extra
import Models.Enigma.OperationMode
import Models.Enigma.Plugboard as Plugboard
import Models.Enigma.Reflector as Reflector
import Models.Enigma.Rotor as Rotor exposing (Rotor, rotor1, rotor2, rotor3, staticRotor)
import Models.Enigma.SubstitutionLog as Log exposing (SubstitutionLog, UpdateLogFunction)
import Random
import Random.List
import Utils.AlphabetHelper
import Utils.Helper


type alias Enigma =
    { rotors : List Rotor
    , reflector : Reflector.Reflector
    , plugBoard : Plugboard.Plugboard
    , operationMode : Models.Enigma.OperationMode.OperationMode
    }


type RandomizationType
    = RandomizePlugboard (List Int)
    | RandomizeRotor ( Int, Rotor )
    | RandomizeReflector Reflector.Reflector
    | RandomizeRotorStartPosition ( Int, Int )
    | RandomizeRotorRingPosition ( Int, Int )


defaultEnigma : Enigma
defaultEnigma =
    { rotors = [ rotor1, rotor2, rotor3 ]
    , reflector = Reflector.reflectorB
    , plugBoard = Plugboard.defaultPlugboard
    , operationMode = Models.Enigma.OperationMode.Configuration
    }



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Rotate the rotors of the enigma and perform the substitution.
If the given Character is not in the alphabet, the result will be nothing and the rotors will not be rotated
-}
performRotationAndSubstitution : Enigma -> Char -> ( Enigma, Maybe SubstitutionLog, Maybe Char )
performRotationAndSubstitution enigma input =
    if Utils.AlphabetHelper.isLetterInAlphabet input then
        let
            rotatedEnigma =
                rotateRotors enigma

            ( maybeLog, maybeResult ) =
                substituteCharacter input rotatedEnigma
        in
        ( rotatedEnigma, maybeLog |> Log.postProcessLog, maybeResult )

    else
        ( enigma, Nothing, Nothing )


{-| Substitute a character with the current enigma configuration.
The will character be replaced with the rotors to the reflector, then with the reflector, and backwards
-}
substituteCharacter : Char -> Enigma -> ( Maybe SubstitutionLog, Maybe Char )
substituteCharacter inputChar enigma =
    let
        buildPairFunction =
            \a b -> ( a, b )
    in
    inputChar
        |> Utils.AlphabetHelper.characterToCharacterIndex
        |> buildPairFunction (Just Log.emptySubstitutionLog)
        |> substituteCharacterWithPlugboard enigma Log.addPlugboardInputSubstitution
        |> substituteCharacterToReflector enigma Log.addRotorToReflectorSubstitution
        |> substituteCharacterWithReflector enigma Log.addReflectorSubstitution
        |> substituteCharacterFromReflector enigma Log.addRotorFromReflectorSubstitution
        |> substituteCharacterWithPlugboard enigma Log.addPlugboardOutputSubstitution
        |> Tuple.mapSecond Utils.AlphabetHelper.characterIndexToCharacter


{-| Set the rotor in the enigma at the given index with the given rotor. The position values will be
copied to new new rotor
-}
setRotor : Enigma -> Int -> Rotor -> Enigma
setRotor enigma rotorPosition newRotor =
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


{-| Set the reflector of the enigma
-}
setReflector : Enigma -> Reflector.Reflector -> Enigma
setReflector enigma newReflector =
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
pressCharOnPlugBoard : Enigma -> Plugboard.CharPosition -> Int -> Enigma
pressCharOnPlugBoard enigma charPosition charIndex =
    { enigma | plugBoard = Plugboard.pressChar enigma.plugBoard charPosition charIndex }


{-| reset the plugboard. Remove all stored connections and set the selected Input/OutputChar to nothing
-}
resetPlugBoard : Enigma -> Enigma
resetPlugBoard enigma =
    { enigma | plugBoard = Plugboard.resetPlugBoard enigma.plugBoard }


{-| Set the currentPosition in each rotor to the selected startPosition
-}
setStartPositionAsCurrentPosition : Enigma -> Enigma
setStartPositionAsCurrentPosition enigma =
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
randomizeReflectorCmd : (Reflector.Reflector -> msg) -> Cmd msg
randomizeReflectorCmd function =
    Dict.values Reflector.getAllReflectors
        |> Random.List.choose
        |> Random.map (Maybe.withDefault Reflector.reflectorB << Tuple.first)
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
handleRandomizeResult : Enigma -> RandomizationType -> Enigma
handleRandomizeResult enigma randomizationType =
    case randomizationType of
        RandomizePlugboard newPlugboardList ->
            { enigma | plugBoard = Plugboard.handleRandomPlugboardCmd enigma.plugBoard newPlugboardList }

        RandomizeRotor ( rotorPosition, newRotor ) ->
            setRotor enigma rotorPosition newRotor

        RandomizeReflector newReflector ->
            setReflector enigma newReflector

        RandomizeRotorStartPosition ( rotorPosition, newStartPosition ) ->
            setStartPositionOfRotor enigma rotorPosition newStartPosition

        RandomizeRotorRingPosition ( rotorPosition, newRingPosition ) ->
            setRingPositionOfRotor enigma rotorPosition newRingPosition



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Replace the character with the plugboard and add the value in the SubstitutionLog
-}
substituteCharacterWithPlugboard : Enigma -> UpdateLogFunction -> ( Maybe SubstitutionLog, Maybe Int ) -> ( Maybe SubstitutionLog, Maybe Int )
substituteCharacterWithPlugboard enigma updateLogFunction ( maybeSubstitutionLog, maybeCharIndex ) =
    case maybeCharIndex of
        Just charIndex ->
            let
                outputChar =
                    Plugboard.substituteCharacter charIndex enigma.plugBoard
            in
            ( updateLogFunction ( charIndex, outputChar ) maybeSubstitutionLog, Just outputChar )

        _ ->
            ( Nothing, Nothing )


{-| Replace the character with the rotors of the enigma in the direction to the reflector and add the values
in the SubstitutionLog
-}
substituteCharacterToReflector : Enigma -> UpdateLogFunction -> ( Maybe SubstitutionLog, Maybe Int ) -> ( Maybe SubstitutionLog, Maybe Int )
substituteCharacterToReflector enigma updateLogFunction logIndexPair =
    Utils.Helper.foldr2
        --    TODO Eta reduction?
        (\( maybeSubstitutionLog, maybeInputIndex ) currentRotor previousRotor ->
            case maybeInputIndex of
                Nothing ->
                    ( Nothing, Nothing )

                Just inputCharIndex ->
                    case Rotor.substituteCharacter Rotor.ToReflector inputCharIndex currentRotor previousRotor of
                        Nothing ->
                            ( Nothing, Nothing )

                        Just outputCharIndex ->
                            ( updateLogFunction (updateRotorReflectorLogPair currentRotor previousRotor ( inputCharIndex, outputCharIndex )) maybeSubstitutionLog, Just outputCharIndex )
        )
        logIndexPair
        enigma.rotors
        staticRotor


{-| Replace the character with the rotors of the enigma in the direction from the reflector and add the values
in the SubstitutionLog
-}
substituteCharacterFromReflector : Enigma -> UpdateLogFunction -> ( Maybe SubstitutionLog, Maybe Int ) -> ( Maybe SubstitutionLog, Maybe Int )
substituteCharacterFromReflector enigma updateLogFunction logIndexPair =
    Utils.Helper.foldl2
        --    TODO Eta reduction
        (\( maybeSubstitutionLog, maybeInputIndex ) currentRotor previousRotor ->
            case maybeInputIndex of
                Nothing ->
                    ( Nothing, Nothing )

                Just inputCharIndex ->
                    case Rotor.substituteCharacter Rotor.FromReflector inputCharIndex currentRotor previousRotor of
                        Nothing ->
                            ( Nothing, Nothing )

                        Just outputCharIndex ->
                            ( updateLogFunction (updateRotorReflectorLogPair currentRotor previousRotor ( inputCharIndex, outputCharIndex )) maybeSubstitutionLog, Just outputCharIndex )
        )
        logIndexPair
        enigma.rotors
        staticRotor


{-| Replace the character with the reflector of the enigma and add the value in the SubstitutionLog
-}
substituteCharacterWithReflector : Enigma -> UpdateLogFunction -> ( Maybe SubstitutionLog, Maybe Int ) -> ( Maybe SubstitutionLog, Maybe Int )
substituteCharacterWithReflector enigma updateLogFunction ( maybeSubstitutionLog, maybeInputCharIndex ) =
    case maybeInputCharIndex of
        Nothing ->
            ( Nothing, Nothing )

        Just inputCharIndex ->
            case Reflector.substituteCharacter inputCharIndex enigma.reflector of
                Nothing ->
                    ( Nothing, Nothing )

                Just outputCharIndex ->
                    ( updateLogFunction ( inputCharIndex, outputCharIndex ) maybeSubstitutionLog, Just outputCharIndex )


{-| Rotate the rotors in the enigma.
The result is the enigma with updated rotors
-}
rotateRotors : Enigma -> Enigma
rotateRotors enigma =
    { enigma | rotors = Utils.Helper.map2 enigma.rotors rotateRotor2 True }


{-| Rotate the currentRotor if it is required.
The rotor will be rotated when the bool is True (this indicates that the previous rotor turns the current rotor)
OR the double stepping occurs. Double Stepping occurs, when a rotor in the middle is in its notch position and the
first rotor is rotating (the first rotor is rotating each step). For further information visit :
<http://www.intelligenia.org/downloads/rotors1.pdf> or <https://en.wikipedia.org/wiki/Enigma_rotor_details>
-}
rotateRotor2 : Rotor -> Maybe Rotor -> Bool -> ( Bool, Rotor )
rotateRotor2 currentRotor maybeNextRotor shouldRotate =
    let
        rotatedResult =
            ( List.member currentRotor.currentPosition currentRotor.notches, Rotor.rotateRotor currentRotor )

        notRotatedResult =
            ( False, currentRotor )
    in
    case ( shouldRotate, maybeNextRotor ) of
        ( True, _ ) ->
            rotatedResult

        -- Double Stepping: For further information see: http://www.intelligenia.org/downloads/rotors1.pdf
        -- or https://en.wikipedia.org/wiki/Enigma_rotor_details
        ( _, Just _ ) ->
            if List.member currentRotor.currentPosition currentRotor.notches then
                rotatedResult

            else
                notRotatedResult

        ( _, _ ) ->
            notRotatedResult


{-| Randomize the rotor for the given index
function - parse the index and the rotor to a msg
rotorIndex - the index of the selected rotor
-}
randomizeRotor : (( Int, Rotor ) -> msg) -> Int -> Cmd msg
randomizeRotor function rotorIndex =
    Random.List.choose (Dict.values Rotor.getAllRotors)
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


{-| update the input output index with the ring and rotor position of the current and previous rotor.
This is required to display the log correctly
-}
updateRotorReflectorLogPair : Rotor -> Rotor -> ( Int, Int ) -> ( Int, Int )
updateRotorReflectorLogPair currentRotor previousRotor ( inputIndex, outputIndex ) =
    let
        updatedInputIndex =
            inputIndex - previousRotor.currentPosition + previousRotor.ringPosition |> modBy 26

        updatedOutputIndex =
            outputIndex - currentRotor.currentPosition + currentRotor.ringPosition |> modBy 26
    in
    ( updatedInputIndex, updatedOutputIndex )
