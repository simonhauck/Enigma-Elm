module Models.Enigma.Reflector exposing
    ( Reflector
    , getAllReflectors
    , reflectorA
    , reflectorB
    , reflectorC
    , substituteCharacter
    )

import Dict exposing (Dict)
import List.Extra


{-| -}
type alias Reflector =
    { name : String
    , characterSequence : List Int
    }


{-| Substitute a character with the given rotor.
The index is the position of the character in the alphabet
The reflector is used to replace the character
-}
substituteCharacter : Int -> Reflector -> Maybe Int
substituteCharacter index reflector =
    List.Extra.getAt index reflector.characterSequence


{-| Get a Dic with all available reflectors.
The key is the name of the reflector.
The value is the reflector with the defined fields.
-}
getAllReflectors : Dict String Reflector
getAllReflectors =
    Dict.fromList
        [ ( reflectorA.name, reflectorA )
        , ( reflectorB.name, reflectorB )
        , ( reflectorC.name, reflectorC )
        ]


reflectorA : Reflector
reflectorA =
    { name = "Reflector A"
    , characterSequence = [ 4, 9, 12, 25, 0, 11, 24, 23, 21, 1, 22, 5, 2, 17, 16, 20, 14, 13, 19, 18, 15, 8, 10, 7, 6, 3 ]
    }


reflectorB : Reflector
reflectorB =
    { name = "Reflector B"
    , characterSequence = [ 24, 17, 20, 7, 16, 18, 11, 3, 15, 23, 13, 6, 14, 10, 12, 8, 4, 1, 5, 25, 2, 22, 21, 9, 0, 19 ]
    }


reflectorC : Reflector
reflectorC =
    { name = "Reflector C"
    , characterSequence = [ 5, 21, 15, 9, 8, 0, 14, 24, 4, 3, 17, 25, 23, 22, 6, 2, 19, 10, 20, 16, 18, 1, 13, 12, 7, 11 ]
    }
