module Enigma.Rotor exposing (Rotor, SignalDirection(..), getAllRotors, rotateRotor, rotor1, rotor2, rotor3, rotor4, rotor5, rotor6, rotor7, rotor8, staticRotor, substituteCharacter)

import Array exposing (Array)
import Dict exposing (Dict)
import Utils.AlphabetHelper


{-| Definition of a rotor in the enigma.
startPosition - is the initial Position of the rotor
ringPosition - is the rotation of the ring
currentPosition - is the current position of the rotor
turningPoints - indices where the next rotor must be turned
characterSequence - the character sequence of the rotor
-}
type alias Rotor =
    { startPosition : Int
    , ringPosition : Int
    , currentPosition : Int
    , turningPoints : List Int
    , characterSequence : Array Char
    }


type SignalDirection
    = ToReflector
    | FromReflector


{-| Get a Dict with all available rotors.
The key is the name of the rotor
The value is the rotor with the defined fields
-}
getAllRotors : Dict String Rotor
getAllRotors =
    Dict.fromList
        [ ( "Rotor I", rotor1 )
        , ( "Rotor II", rotor2 )
        , ( "Rotor III", rotor3 )
        , ( "Rotor IV", rotor4 )
        , ( "Rotor V", rotor5 )
        , ( "Rotor VI", rotor6 )
        , ( "Rotor VII", rotor7 )
        , ( "Rotor VIII", rotor8 )
        ]


substituteCharacter : Char -> Rotor -> Rotor -> SignalDirection -> Maybe Char
substituteCharacter inputChar currentRotor previousRotor signalDirection =
    case signalDirection of
        ToReflector ->
            substituteCharacterToReflector inputChar currentRotor previousRotor

        FromReflector ->
            substituteCharacterFromReflector inputChar currentRotor previousRotor


{-| Substitute a character in direction to the reflector with the current and previous rotor
-}
substituteCharacterToReflector : Char -> Rotor -> Rotor -> Maybe Char
substituteCharacterToReflector inputChar currentRotor previousRotor =
    case Utils.AlphabetHelper.indexOfCharacterInSequence inputChar Utils.AlphabetHelper.alphabetSequence of
        Just index ->
            Array.get (modBy 26 (index + currentRotor.currentPosition - previousRotor.currentPosition)) currentRotor.characterSequence

        Nothing ->
            Nothing


substituteCharacterFromReflector : Char -> Rotor -> Rotor -> Maybe Char
substituteCharacterFromReflector inputChar currentRotor previousRotor =
    let
        differenceRotors =
            currentRotor.currentPosition - previousRotor.currentPosition

        correctedInputCharResult =
            Utils.AlphabetHelper.increaseAlphabetCharacter inputChar differenceRotors
    in
    case correctedInputCharResult of
        Just correctedInput ->
            case Utils.AlphabetHelper.indexOfCharacterInSequence correctedInput currentRotor.characterSequence of
                Just index ->
                    Array.get (modBy 26 index) Utils.AlphabetHelper.alphabetSequence

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


rotateRotor : Rotor -> Rotor
rotateRotor rotor =
    { rotor | currentPosition = modBy (Array.length rotor.characterSequence) (rotor.currentPosition + 1) }


staticRotor =
    { startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = []
    , characterSequence = Utils.AlphabetHelper.alphabetSequence
    }


rotor1 =
    { startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 16 ]
    , characterSequence = Array.fromList (String.toList "EKMFLGDQVZNTOWYHXUSPAIBRCJ")
    }


rotor2 =
    { startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 4 ]
    , characterSequence = Array.fromList (String.toList "AJDKSIRUXBLHWTMCQGZNPYFVOE")
    }


rotor3 =
    { startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 21 ]
    , characterSequence = Array.fromList (String.toList "BDFHJLCPRTXVZNYEIWGAKMUSQO")
    }


rotor4 =
    { startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 9 ]
    , characterSequence = Array.fromList (String.toList "ESOVPZJAYQUIRHXLNFTGKDCMWB")
    }


rotor5 =
    { startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 25 ]
    , characterSequence = Array.fromList (String.toList "VZBRGITYUPSDNHLXAWMJQOFECK")
    }


rotor6 =
    { startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 25, 12 ]
    , characterSequence = Array.fromList (String.toList "JPGVOUMFYQBENHZRDKASXLICTW")
    }


rotor7 =
    { startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 25, 12 ]
    , characterSequence = Array.fromList (String.toList "NZJHGRCXMYSWBOUFAIVLPEKQDT")
    }


rotor8 =
    { startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 25, 12 ]
    , characterSequence = Array.fromList (String.toList "FKQHTLXOCBJSPDZRAMEWNIUYGV")
    }
