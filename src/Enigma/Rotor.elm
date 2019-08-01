module Enigma.Rotor exposing (Rotor, SignalDirection(..), getAllRotors, rotateRotor, rotor1, rotor2, rotor3, rotor4, rotor5, rotor6, rotor7, rotor8, staticRotor, substituteCharacter)

import Dict exposing (Dict)
import List.Extra
import Utils.AlphabetHelper


{-| Definition of a rotor in the enigma.
startPosition - is the initial Position of the rotor
ringPosition - is the rotation of the ring
currentPosition - is the current position of the rotor
turningPoints - indices where the next rotor must be turned
characterSequence - the character sequence of the rotor. The characters are represented by integers. The value is the position
of the character in the alphabet
-}
type alias Rotor =
    { name : String
    , startPosition : Int
    , ringPosition : Int
    , currentPosition : Int
    , turningPoints : List Int
    , characterSequence : List Int
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
        [ ( rotor1.name, rotor1 )
        , ( rotor2.name, rotor2 )
        , ( rotor3.name, rotor3 )
        , ( rotor4.name, rotor4 )
        , ( rotor5.name, rotor5 )
        , ( rotor6.name, rotor6 )
        , ( rotor7.name, rotor7 )
        , ( rotor8.name, rotor8 )
        ]


{-| Substitute a character with a rotor.
The SignalDirection indicates if the signal is in direction to the rotor or from the rotor
The int value is the position of the character in the alphabet
The first rotor is the current rotor used to replace the character
The second rotor is the previous used rotor
-}
substituteCharacter : SignalDirection -> Int -> Rotor -> Rotor -> Maybe Int
substituteCharacter signalDirection =
    case signalDirection of
        ToReflector ->
            substituteCharacterToReflector

        FromReflector ->
            substituteCharacterFromReflector


{-| Substitute a character in direction to the reflector with the current and previous rotor
The index is the position of the character in the alphabet
The first rotor is the current rotor, which is used to replace the character
The second rotor is the previous rotor used to replace a char
-}
substituteCharacterToReflector : Int -> Rotor -> Rotor -> Maybe Int
substituteCharacterToReflector index currentRotor previousRotor =
    List.Extra.getAt (modBy 26 (index + currentRotor.currentPosition - previousRotor.currentPosition)) currentRotor.characterSequence


{-| Substitute a character in direction from the reflector with the current and previous rotor
The index is the position of the character in the alphabet
The first rotor is the current rotor, which is used to replace the character
The second rotor is the previous rotor used to replace a char
-}
substituteCharacterFromReflector : Int -> Rotor -> Rotor -> Maybe Int
substituteCharacterFromReflector index currentRotor previousRotor =
    let
        differenceRotors =
            currentRotor.currentPosition - previousRotor.currentPosition

        correctedIndex =
            modBy 26 (index + differenceRotors)
    in
    List.Extra.elemIndex correctedIndex currentRotor.characterSequence


{-| Rotate the rotor by one.
-}
rotateRotor : Rotor -> Rotor
rotateRotor rotor =
    { rotor | currentPosition = modBy (List.length rotor.characterSequence) (rotor.currentPosition + 1) }


staticRotor =
    { name = "staticRotor"
    , startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = []
    , characterSequence = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ]
    }


rotor1 =
    { name = "Rotor I"
    , startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 16 ]
    , characterSequence = [ 4, 10, 12, 5, 11, 6, 3, 16, 21, 25, 13, 19, 14, 22, 24, 7, 23, 20, 18, 15, 0, 8, 1, 17, 2, 9 ]
    }


rotor2 =
    { name = "Rotor II"
    , startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 4 ]
    , characterSequence = [ 0, 9, 3, 10, 18, 8, 17, 20, 23, 1, 11, 7, 22, 19, 12, 2, 16, 6, 25, 13, 15, 24, 5, 21, 14, 4 ]
    }


rotor3 =
    { name = "Rotor III"
    , startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 21 ]
    , characterSequence = [ 1, 3, 5, 7, 9, 11, 2, 15, 17, 19, 23, 21, 25, 13, 24, 4, 8, 22, 6, 0, 10, 12, 20, 18, 16, 14 ]
    }


rotor4 =
    { name = "Rotor IV"
    , startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 9 ]
    , characterSequence = [ 4, 18, 14, 21, 15, 25, 9, 0, 24, 16, 20, 8, 17, 7, 23, 11, 13, 5, 19, 6, 10, 3, 2, 12, 22, 1 ]
    }


rotor5 =
    { name = "Rotor V"
    , startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 25 ]
    , characterSequence = [ 21, 25, 1, 17, 6, 8, 19, 24, 20, 15, 18, 3, 13, 7, 11, 23, 0, 22, 12, 9, 16, 14, 5, 4, 2, 10 ]
    }


rotor6 =
    { name = "Rotor VI"
    , startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 25, 12 ]
    , characterSequence = [ 9, 15, 6, 21, 14, 20, 12, 5, 24, 16, 1, 4, 13, 7, 25, 17, 3, 10, 0, 18, 23, 11, 8, 2, 19, 22 ]
    }


rotor7 =
    { name = "Rotor VII"
    , startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 25, 12 ]
    , characterSequence = [ 13, 25, 9, 7, 6, 17, 2, 23, 12, 24, 18, 22, 1, 14, 20, 5, 0, 8, 21, 11, 15, 4, 10, 16, 3, 19 ]
    }


rotor8 =
    { name = "Rotor VIII"
    , startPosition = 0
    , ringPosition = 0
    , currentPosition = 0
    , turningPoints = [ 25, 12 ]
    , characterSequence = [ 5, 10, 16, 7, 19, 11, 23, 14, 2, 1, 9, 18, 15, 3, 25, 17, 0, 12, 4, 22, 13, 8, 20, 24, 6, 21 ]
    }
