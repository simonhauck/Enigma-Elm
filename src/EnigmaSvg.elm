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


type alias Color =
    String


type alias LineStrokeWidth =
    String


debugLog =
    { plugboardInputSubstitution = ( 0, 0 )
    , plugboardOutputSubstitution = ( 1, 1 )
    , reflectorSubstitution = ( 5, 18 )
    , rotorToReflectorSubstitution = [ ( 0, 2 ), ( 2, 3 ), ( 3, 5 ) ]
    , rotorFromReflectorSubstitution = [ ( 18, 18 ), ( 18, 4 ), ( 4, 1 ) ]
    }



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


enigmaSvg : Enigma.EnigmaMachine.Enigma -> Html msg
enigmaSvg enigma =
    let
        yCoordinate =
            20

        reflectorXCoordinate =
            175

        rotorXCoordinate =
            reflectorXCoordinate + spaceBetweenReflectorAndRotor

        plugBoardXCoordinate =
            (spaceBetweenRotors + rotorWidth) * List.length enigma.rotors + rotorXCoordinate
    in
    Svg.svg
        [ Svg.Attributes.width "10000"
        , Svg.Attributes.height "1000"
        ]
        (drawReflector enigma.reflector reflectorXCoordinate yCoordinate
            ++ drawRotors enigma.rotors rotorXCoordinate yCoordinate
            ++ drawPlugBoard enigma.plugBoard plugBoardXCoordinate yCoordinate
            ++ drawSubstitutionLog debugLog yCoordinate reflectorXCoordinate rotorXCoordinate plugBoardXCoordinate
        )



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| space between the components in a row
-}
rowYSpace =
    25


{-| space between the left rotor and and the reflector
-}
spaceBetweenReflectorAndRotor =
    125


{-| space between two rotors
-}
spaceBetweenRotors =
    125


{-| width of a rotor
-}
rotorWidth =
    200


{-| space between the plugboard rows
-}
plugboardWidth =
    100


{-| default color for connection lines
-}
defaultColor =
    "red"


{-| color for active connections to the reflector
-}
colorToReflector =
    "blue"


{-| color for active connections from the reflector
-}
colorFromReflector =
    "green"


{-| default stroke width for connection lines
-}
defaultLineStrokeWidth =
    "2"


{-| stroke width for active connections
-}
connectionLineStrokeWidth =
    "4"


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
    connectionLines ++ drawAlphabetColumn Left 0 x y


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


{-| Draw the plugboard and all of its connections
-}
drawPlugBoard : Enigma.Plugboard.Plugboard -> Int -> Int -> List (Svg msg)
drawPlugBoard plugboard x y =
    let
        connectionLines =
            List.map
                (\inputCharIndex ->
                    let
                        outputCharIndex =
                            Enigma.Plugboard.substituteCharacter inputCharIndex plugboard
                    in
                    drawPlugboardConnection inputCharIndex outputCharIndex x y defaultColor defaultLineStrokeWidth
                )
                (List.range 0 25)
    in
    drawAlphabetColumn Left 0 x y ++ drawAlphabetColumn Right 0 (x + plugboardWidth) y ++ connectionLines


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
                    let
                        rotatedInputIndex =
                            inputIndex - rotor.currentPosition + rotor.ringPosition |> modBy 26

                        rotatedOutputIndex =
                            outputIndex - rotor.currentPosition + rotor.ringPosition |> modBy 26
                    in
                    drawRotorConnection rotatedInputIndex rotatedOutputIndex x y defaultColor defaultLineStrokeWidth :: listAcc
                )
                []
                rotor.characterSequence
    in
    drawAlphabetColumn Left (rotor.currentPosition - rotor.ringPosition) x y
        ++ drawAlphabetColumn Right (rotor.currentPosition - rotor.ringPosition) (x + rotorWidth) y
        ++ connectionLines


drawSubstitutionLog : Enigma.EnigmaMachine.SubstitutionLog -> Int -> Int -> Int -> Int -> List (Svg msg)
drawSubstitutionLog substitutionLog yCoordinate reflectorXCoordinate rotorXCoordinate plugboardXCoordinate =
    let
        plugboardToReflectorArrow =
            drawArrow (plugboardXCoordinate + plugboardWidth)
                (Tuple.first substitutionLog.plugboardInputSubstitution * rowYSpace + yCoordinate)
                True
                colorToReflector

        plugboardFromReflectorArrow =
            drawArrow
                (plugboardXCoordinate + plugboardWidth)
                (Tuple.second substitutionLog.plugboardOutputSubstitution * rowYSpace + yCoordinate)
                False
                colorFromReflector

        plugboardToReflectorConnection =
            drawPlugboardConnection
                (Tuple.first substitutionLog.plugboardInputSubstitution)
                (Tuple.second substitutionLog.plugboardInputSubstitution)
                plugboardXCoordinate
                yCoordinate
                colorToReflector
                connectionLineStrokeWidth

        plugboardFromReflectorConnection =
            drawPlugboardConnection
                (Tuple.second substitutionLog.plugboardOutputSubstitution)
                (Tuple.first substitutionLog.plugboardOutputSubstitution)
                plugboardXCoordinate
                yCoordinate
                colorFromReflector
                connectionLineStrokeWidth

        rotorConnectionsToReflector =
            List.Extra.indexedFoldl
                (\index ( inputCharIndex, outputCharIndex ) listAcc ->
                    [ drawRotorConnection
                        inputCharIndex
                        outputCharIndex
                        (rotorXCoordinate + index * (rotorWidth + spaceBetweenRotors))
                        yCoordinate
                        colorToReflector
                        connectionLineStrokeWidth
                    , drawConnectionBetweenRotors
                        inputCharIndex
                        index
                        rotorXCoordinate
                        yCoordinate
                        colorToReflector
                        connectionLineStrokeWidth
                    ]
                        ++ listAcc
                )
                []
                (List.reverse substitutionLog.rotorToReflectorSubstitution)

        rotorConnectionsFromReflector =
            List.Extra.indexedFoldl
                (\index ( inputCharIndex, outputCharIndex ) listAcc ->
                    [ drawRotorConnection
                        outputCharIndex
                        inputCharIndex
                        (rotorXCoordinate + index * (rotorWidth + spaceBetweenRotors))
                        yCoordinate
                        colorFromReflector
                        connectionLineStrokeWidth
                    , drawConnectionBetweenRotors
                        outputCharIndex
                        index
                        rotorXCoordinate
                        yCoordinate
                        colorFromReflector
                        connectionLineStrokeWidth
                    ]
                        ++ listAcc
                )
                []
                substitutionLog.rotorFromReflectorSubstitution
    in
    plugboardFromReflectorConnection
        :: plugboardToReflectorConnection
        :: plugboardToReflectorArrow
        ++ plugboardFromReflectorArrow
        ++ rotorConnectionsToReflector
        ++ rotorConnectionsFromReflector


{-| draw a column with points and the alphabet letters
shiftLetters - the letters will be shifted with the given number
characterOrientation - display the character on the right or left
x - the xCoordinate of the row
startY - the yCoordinate for the first row
-}
drawAlphabetColumn : CharacterOrientation -> Int -> Int -> Int -> List (Svg msg)
drawAlphabetColumn characterOrientation shiftLetters x startY =
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
                    characterIndex + shiftLetters |> modBy 26 |> Just |> Utils.AlphabetHelper.characterIndexToCharacter |> Maybe.withDefault '-'
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
drawDefaultLine : ( Int, Int ) -> ( Int, Int ) -> Svg msg
drawDefaultLine startPoint endpoint =
    drawLineWithColor startPoint endpoint defaultColor defaultLineStrokeWidth


{-| draw a line between the given coordinates with the given color and strokeWidth
-}
drawLineWithColor : ( Int, Int ) -> ( Int, Int ) -> Color -> LineStrokeWidth -> Svg msg
drawLineWithColor ( x1, y1 ) ( x2, y2 ) color strokeWidth =
    Svg.line
        [ Svg.Attributes.x1 <| String.fromInt x1
        , Svg.Attributes.y1 <| String.fromInt y1
        , Svg.Attributes.x2 <| String.fromInt x2
        , Svg.Attributes.y2 <| String.fromInt y2
        , Svg.Attributes.stroke color
        , Svg.Attributes.strokeWidth strokeWidth
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
    [ drawDefaultLine topRightCorner topLeftCorner
    , drawDefaultLine topLeftCorner bottomLeftCorner
    , drawDefaultLine bottomLeftCorner bottomRightCorner
    ]


{-| draw a connection in a rotor
inputCharIndex - index of the inputChar
outputCharIndex - index of the outputChar
x - xCoordinate of the top left corner where the rotor is drawn
startY - yCoordinate of the top left corner where the rotor is drawn
-}
drawRotorConnection : Int -> Int -> Int -> Int -> Color -> LineStrokeWidth -> Svg msg
drawRotorConnection inputCharIndex outputCharIndex x startY =
    let
        rightPoint =
            ( x + rotorWidth, startY + inputCharIndex * rowYSpace )

        leftPoint =
            ( x, startY + outputCharIndex * rowYSpace )
    in
    drawLineWithColor rightPoint leftPoint


{-| draw a connection between rotors
charIndex - the index of the character where the line will be drawn (the line is always horizontal)
rotorIndex - the index of the rotor
rotorX - the xCoordinate of the rotor elements
rotorY - the yCoordinate of the rotor elements
-}
drawConnectionBetweenRotors : Int -> Int -> Int -> Int -> Color -> LineStrokeWidth -> Svg msg
drawConnectionBetweenRotors charIndex rotorIndex rotorX rotorY =
    let
        startPoint =
            ( rotorX + rotorWidth + rotorIndex * (rotorWidth + spaceBetweenRotors), rotorY + rowYSpace * charIndex )

        endPoint =
            ( rotorX + (rotorIndex + 1) * (rotorWidth + spaceBetweenRotors), rotorY + rowYSpace * charIndex )
    in
    drawLineWithColor startPoint endPoint


{-| draw a connection in the plugboard
-}
drawPlugboardConnection : Int -> Int -> Int -> Int -> Color -> LineStrokeWidth -> Svg msg
drawPlugboardConnection inputCharIndex outputCharIndex x startY =
    let
        rightPoint =
            ( x + plugboardWidth, startY + inputCharIndex * rowYSpace )

        leftPoint =
            ( x, startY + outputCharIndex * rowYSpace )
    in
    drawLineWithColor rightPoint leftPoint


{-| Draw an arrow at the given coordinate.
x - the x coordinate of the arrow
y - the y coordinate of the arrow
arrowLeft - set the direction of the arrow.
color - of the arrow
-}
drawArrow : Int -> Int -> Bool -> Color -> List (Svg msg)
drawArrow x y arrowLeft color =
    let
        arrowLength =
            75

        arrowOffset =
            10

        tipYCoordinate =
            10

        tipXCoordinate =
            10

        startPoint =
            if arrowLeft then
                ( x + arrowOffset, y )

            else
                ( x + arrowOffset + arrowLength, y )

        endPoint =
            if arrowLeft then
                ( x + arrowOffset + arrowLength, y )

            else
                ( x + arrowOffset, y )

        topPoint =
            if arrowLeft then
                ( x + arrowOffset + tipXCoordinate, y + tipYCoordinate )

            else
                ( x + arrowOffset + arrowLength - tipXCoordinate, y + tipYCoordinate )

        bottomPoint =
            if arrowLeft then
                ( x + arrowOffset + tipXCoordinate, y - tipYCoordinate )

            else
                ( x + arrowOffset + arrowLength - tipXCoordinate, y - tipYCoordinate )
    in
    [ drawLineWithColor startPoint endPoint color defaultLineStrokeWidth
    , drawLineWithColor startPoint topPoint color defaultLineStrokeWidth
    , drawLineWithColor startPoint bottomPoint color defaultLineStrokeWidth
    ]
