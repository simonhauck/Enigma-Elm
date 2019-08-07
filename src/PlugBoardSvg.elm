module PlugBoardSvg exposing (plugBoardCanvas)

import Enigma.Plugboard
import Html exposing (Html)
import Svg
import Svg.Attributes


plugBoardCanvas : Enigma.Plugboard.Plugboard -> Int -> Html msg
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


drawCircles : Enigma.Plugboard.Plugboard -> Int -> Int -> Int -> List (Svg.Svg msg)
drawCircles plugboard height heightOffset widthPerCharacter =
    let
        generateStartListFunction =
            Maybe.map (\selectedInput -> [ selectedInput ]) >> Maybe.withDefault []

        ( resultInputList, resultOutputList ) =
            List.foldr
                (\( input, output ) ( inputListAcc, outputList ) ->
                    ( input :: inputListAcc, output :: outputList )
                )
                ( generateStartListFunction plugboard.selectedInputChar, generateStartListFunction plugboard.selectedOutputChar )
                plugboard.switchedCharsList
    in
    drawCircleRow heightOffset widthPerCharacter resultInputList
        ++ drawCircleRow (height - heightOffset) widthPerCharacter resultOutputList


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
                "red"

             else
                "none"
            )
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "2"
        ]
        []


drawLines : Enigma.Plugboard.Plugboard -> Int -> Int -> Int -> List (Svg.Svg msg)
drawLines plugboard height heightOffset widthPerCharacter =
    List.map (\characterPair -> drawLineBetweenCharacters characterPair height heightOffset widthPerCharacter) plugboard.switchedCharsList


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
        , Svg.Attributes.stroke "red"
        , Svg.Attributes.strokeWidth "3"
        ]
        []
