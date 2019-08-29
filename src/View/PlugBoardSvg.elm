module View.PlugBoardSvg exposing (plugBoardCanvas)

import Html exposing (Html)
import Models.Enigma.Plugboard as Plugboard
import Svg
import Svg.Attributes



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


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


drawCircleRow : Int -> Int -> List Int -> List (Svg.Svg msg)
drawCircleRow heightCoordinate widthPerCharacter selectedChars =
    let
        offset =
            round (toFloat widthPerCharacter / 2)
    in
    List.map (\index -> circleForCharacter ( offset + widthPerCharacter * index, heightCoordinate ) (List.member index selectedChars)) (List.range 0 25)


circleForCharacter : ( Int, Int ) -> Bool -> Svg.Svg msg
circleForCharacter ( x, y ) fillCircle =
    Svg.circle
        [ Svg.Attributes.cx (String.fromInt x)
        , Svg.Attributes.cy (String.fromInt y)
        , Svg.Attributes.r "10"
        , Svg.Attributes.fill
            (if fillCircle then
                "#3c8c6e"

             else
                "none"
            )
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "2"
        ]
        []


drawLines : Plugboard.Plugboard -> Int -> Int -> Int -> List (Svg.Svg msg)
drawLines plugboard height heightOffset widthPerCharacter =
    List.foldl
        (\linePair resultList ->
            drawLinePair linePair height heightOffset widthPerCharacter
                ++ resultList
        )
        []
        plugboard.switchedCharsList


drawLinePair : ( Int, Int ) -> Int -> Int -> Int -> List (Svg.Svg msg)
drawLinePair ( inputChar, outputChar ) height heightOffset widthPerCharacter =
    let
        firstLine =
            drawLineBetweenCharacters ( inputChar, outputChar ) height heightOffset widthPerCharacter

        secondLine =
            drawLineBetweenCharacters ( outputChar, inputChar ) height heightOffset widthPerCharacter
    in
    [ firstLine, secondLine ]


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
        , Svg.Attributes.stroke "#3c8c6e"
        , Svg.Attributes.strokeWidth "3"
        ]
        []
