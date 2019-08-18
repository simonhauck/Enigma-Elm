module EnigmaSvg exposing (enigmaSvg)

import Enigma.EnigmaMachine
import Enigma.Plugboard
import Enigma.Reflector
import Enigma.Rotor
import Html exposing (Html)
import List.Extra
import Svg exposing (Svg)
import Svg.Attributes
import Utils.AlphabetHelper


type CharacterOrientation
    = Left
    | Right



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


enigmaSvg : Enigma.EnigmaMachine.Enigma -> Html msg
enigmaSvg enigma =
    Svg.svg
        [ Svg.Attributes.width "10000"
        , Svg.Attributes.height "1000"
        ]
        (drawReflector enigma.reflector 175 20
            ++ drawRotors enigma.rotors 300 20
        )



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| space between the components in a row
-}
rowYSpace =
    25


{-| space between two rotors
-}
spaceBetweenRotors =
    125


{-| width of a rotor
-}
rotorWidth =
    200


{-| Get a list with Svg elements that display the given reflector
reflector - that should be displayed int he svg
x - xPosition of the reflector
y - yPosition of the reflector
-}
drawReflector : Enigma.Reflector.Reflector -> Int -> Int -> List (Svg msg)
drawReflector reflector x y =
    let
        connectionLineMinLength =
            40

        connectionLineLengthPerStep =
            3

        connectionLines =
            List.Extra.indexedFoldl
                (\inputIndex outputIndex listAcc ->
                    if inputIndex <= outputIndex then
                        drawReflectorConnection inputIndex
                            outputIndex
                            x
                            y
                            (List.length listAcc * connectionLineLengthPerStep + connectionLineMinLength)
                            ++ listAcc

                    else
                        listAcc
                )
                []
                reflector.characterSequence
    in
    connectionLines ++ drawAlphabetColumn Left x y


{-| Draw all rotors in the given list
rotors - the rotors of the enigma
x - the xCoordinate in the top left corner where the rotors will be drawn
y - the yCoordinate in the top left corner where the rotors will be drawn
-}
drawRotors : List Enigma.Rotor.Rotor -> Int -> Int -> List (Svg msg)
drawRotors rotors x y =
    List.Extra.indexedFoldl
        (\index rotor listAcc ->
            let
                xCoordinateRotor =
                    x + (index * (spaceBetweenRotors + rotorWidth))
            in
            drawRotor rotor xCoordinateRotor y ++ listAcc
        )
        []
        rotors


{-| Draw the given rotor
rotor - that will be drawn
x - the xCoordinate of the canvas where the rotor will be drawn
y - the yCoordinate of the canvas where the rotor will be drawn
-}
drawRotor : Enigma.Rotor.Rotor -> Int -> Int -> List (Svg msg)
drawRotor rotor x y =
    let
        connectionLines =
            List.Extra.indexedFoldl
                (\inputIndex outputIndex listAcc ->
                    drawRotorConnection inputIndex outputIndex x y :: listAcc
                )
                []
                rotor.characterSequence
    in
    drawAlphabetColumn Left x y
        ++ drawAlphabetColumn Right (x + rotorWidth) y
        ++ connectionLines


{-| draw a column with points and the alphabet letters
characterOrientation - display the character on the right or left
x - the xCoordinate of the row
startY - the yCoordinate for the first row
-}
drawAlphabetColumn : CharacterOrientation -> Int -> Int -> List (Svg msg)
drawAlphabetColumn characterOrientation x startY =
    let
        list =
            List.range 0 25
    in
    List.foldl
        (\characterIndex listAcc ->
            let
                y =
                    startY + characterIndex * rowYSpace

                char =
                    Just characterIndex |> Utils.AlphabetHelper.characterIndexToCharacter |> Maybe.withDefault '-'
            in
            drawCircleCharacterRow characterOrientation x y char ++ listAcc
        )
        []
        list


{-| draw a circle and place a character next to it
characterOrientation - display the character in the right or left
x - the xCoordinate of the row
y - the yCoordinate of the row
inputChar - the character that should be displayed next to the circle
-}
drawCircleCharacterRow : CharacterOrientation -> Int -> Int -> Char -> List (Svg msg)
drawCircleCharacterRow characterOrientation x y inputChar =
    let
        characterOffsetX =
            case characterOrientation of
                Left ->
                    -25

                Right ->
                    25

        characterOffsetY =
            -5
    in
    [ drawSmallCircles x y
    , drawCharacter (x + characterOffsetX) (y + characterOffsetY) inputChar
    ]


{-| draw a character at the given position
x - the xCoordinate
y - the yCoordinate
char - the char that will be written
-}
drawCharacter : Int -> Int -> Char -> Svg msg
drawCharacter x y char =
    Svg.text_
        [ Svg.Attributes.x (String.fromInt x)
        , Svg.Attributes.y (String.fromInt y)
        ]
        [ Svg.text (String.fromChar char) ]


{-| draw a circle at the given coordinate
x - the xCoordinate of the circle
y - the yCoordinate of the circle
-}
drawSmallCircles : Int -> Int -> Svg svg
drawSmallCircles x y =
    Svg.circle
        [ Svg.Attributes.cx (String.fromInt x)
        , Svg.Attributes.cy (String.fromInt y)
        , Svg.Attributes.r "4"
        , Svg.Attributes.fill "red"
        ]
        []


{-| draw a line between the given two points
(x1, y1) - the x and y coordinate of the first point
(x2, y2) - the x and y coordinate of the second point
-}
drawLine : ( Int, Int ) -> ( Int, Int ) -> Svg msg
drawLine ( x1, y1 ) ( x2, y2 ) =
    Svg.line
        [ Svg.Attributes.x1 <| String.fromInt x1
        , Svg.Attributes.y1 <| String.fromInt y1
        , Svg.Attributes.x2 <| String.fromInt x2
        , Svg.Attributes.y2 <| String.fromInt y2
        , Svg.Attributes.stroke "red"
        , Svg.Attributes.strokeWidth "2"
        ]
        []


{-| draw a connection for the reflector
inputCharIndex - the index of the inputChar in the alphabet
outputCharIndex - the index of the outputChar in the alphabet
x - the xCoordinate of the reflector
startY - the yCoordinate of the first reflector row
horizontalLength - the horizontal length of the connection
-}
drawReflectorConnection : Int -> Int -> Int -> Int -> Int -> List (Svg msg)
drawReflectorConnection inputCharIndex outputCharIndex x startY horizontalLength =
    let
        topRightCorner =
            ( x, startY + inputCharIndex * rowYSpace )

        topLeftCorner =
            ( x - horizontalLength, startY + inputCharIndex * rowYSpace )

        bottomLeftCorner =
            ( x - horizontalLength, startY + outputCharIndex * rowYSpace )

        bottomRightCorner =
            ( x, startY + outputCharIndex * rowYSpace )
    in
    [ drawLine topRightCorner topLeftCorner
    , drawLine topLeftCorner bottomLeftCorner
    , drawLine bottomLeftCorner bottomRightCorner
    ]


{-| draw a connection in a rotor
inputCharIndex - index of the inputChar
outputCharIndex - index of the outputChar
x - xCoordinate of the top left corner where the rotor is drawn
startY - yCoordinate of the top left corner where the rotor is drawn
-}
drawRotorConnection : Int -> Int -> Int -> Int -> Svg msg
drawRotorConnection inputCharIndex outputCharIndex x startY =
    let
        topRightPoint =
            ( x + rotorWidth, startY + inputCharIndex * rowYSpace )

        bottomLeftPoint =
            ( x, startY + outputCharIndex * rowYSpace )
    in
    drawLine topRightPoint bottomLeftPoint
