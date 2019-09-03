module View.PlugBoardSvg exposing (plugBoardCanvas)

import Html exposing (Html)
import Models.Enigma.Plugboard as Plugboard
import Svg
import Svg.Attributes
import View.StyleElements



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Draw the given Plugboard on an Svg and return the result as a html element
widthPerCharacter - The width per single character on the svg
-}
plugBoardCanvas : Plugboard.Plugboard -> Int -> Html msg
plugBoardCanvas plugboard widthPerCharacter =
    let
        height =
            200

        heightOffset =
            20
    in
    Svg.svg
        [ Svg.Attributes.width (String.fromInt (widthPerCharacter * 27))
        , Svg.Attributes.height (String.fromInt height)
        ]
        (drawCircles plugboard height heightOffset widthPerCharacter
            ++ drawLines plugboard height heightOffset widthPerCharacter
        )



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Draw two rows of circles for each character in the alphabet
-}
drawCircles : Plugboard.Plugboard -> Int -> Int -> Int -> List (Svg.Svg msg)
drawCircles plugboard height heightOffset widthPerCharacter =
    let
        listFromMaybeFunction =
            Maybe.map (\selectedInput -> [ selectedInput ]) >> Maybe.withDefault []

        ( inputCharList, outputCharList ) =
            List.unzip plugboard.switchedCharsList

        filledCircleList =
            inputCharList
                ++ outputCharList
                ++ listFromMaybeFunction plugboard.selectedInputChar
                ++ listFromMaybeFunction plugboard.selectedOutputChar
    in
    drawCircleRow heightOffset widthPerCharacter filledCircleList
        ++ drawCircleRow (height - heightOffset) widthPerCharacter filledCircleList


{-| Draw a circle for each character in the alphabet.
heightCoordinate - the y coordinate for all circle
widthPerCharacter - the horizontal space for each character
selectedChars - the chars with a filled circle
-}
drawCircleRow : Int -> Int -> List Int -> List (Svg.Svg msg)
drawCircleRow heightCoordinate widthPerCharacter selectedChars =
    let
        offset =
            round (toFloat widthPerCharacter / 2)
    in
    List.map (\index -> circleForCharacter ( offset + widthPerCharacter * index, heightCoordinate ) (List.member index selectedChars)) (List.range 0 25)


{-| Draw a circle at the given x and y coordinate and fill the circle according to the boolean value
-}
circleForCharacter : ( Int, Int ) -> Bool -> Svg.Svg msg
circleForCharacter ( x, y ) fillCircle =
    Svg.circle
        [ Svg.Attributes.cx (String.fromInt x)
        , Svg.Attributes.cy (String.fromInt y)
        , Svg.Attributes.r "10"
        , Svg.Attributes.fill
            (if fillCircle then
                View.StyleElements.primaryColor

             else
                "none"
            )
        , Svg.Attributes.stroke View.StyleElements.thirdColor
        , Svg.Attributes.strokeWidth "2"
        ]
        []


{-| Draw all lines in the plugboard connection
-}
drawLines : Plugboard.Plugboard -> Int -> Int -> Int -> List (Svg.Svg msg)
drawLines plugboard height heightOffset widthPerCharacter =
    List.foldl
        (\linePair resultList ->
            drawLinePair linePair height heightOffset widthPerCharacter
                ++ resultList
        )
        []
        plugboard.switchedCharsList


{-| Draw a line pair between the two given characters.
inputOutputPair - the index of the input and outputChar in the alphabet
height - the height between the two characters
heightOffset - the offset to the top
widthPerCharacter - the space for each character
-}
drawLinePair : ( Int, Int ) -> Int -> Int -> Int -> List (Svg.Svg msg)
drawLinePair ( inputChar, outputChar ) height heightOffset widthPerCharacter =
    let
        firstLine =
            drawLineBetweenCharacters ( inputChar, outputChar ) height heightOffset widthPerCharacter

        secondLine =
            drawLineBetweenCharacters ( outputChar, inputChar ) height heightOffset widthPerCharacter
    in
    [ firstLine, secondLine ]


{-| Draw a line between the two given characters.
inputOutputPair - the index of the input and outputChar in the alphabet
height - the height between the two characters
heightOffset - the offset to the top
widthPerCharacter - the space for each character
-}
drawLineBetweenCharacters : ( Int, Int ) -> Int -> Int -> Int -> Svg.Svg msg
drawLineBetweenCharacters ( inputChar, outputChar ) height heightOffset widthPerCharacter =
    let
        widthOffset =
            round (toFloat widthPerCharacter / 2)

        ( startX, startY ) =
            ( inputChar * widthPerCharacter + widthOffset, heightOffset )

        ( endX, endY ) =
            ( outputChar * widthPerCharacter + widthOffset, height - heightOffset )
    in
    Svg.line
        [ Svg.Attributes.x1 <| String.fromInt startX
        , Svg.Attributes.y1 <| String.fromInt startY
        , Svg.Attributes.x2 <| String.fromInt endX
        , Svg.Attributes.y2 <| String.fromInt endY
        , Svg.Attributes.stroke View.StyleElements.primaryColor
        , Svg.Attributes.strokeWidth "3"
        ]
        []
