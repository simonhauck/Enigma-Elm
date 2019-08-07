module PlugBoardSvg exposing (plugBoardCanvas)

import Enigma.Plugboard
import Html exposing (Html)
import Svg
import Svg.Attributes


plugBoardCanvas : Enigma.Plugboard.Plugboard -> Int -> Html msg
plugBoardCanvas plugboard sizePerCharacter =
    let
        height =
            200

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
    Svg.svg
        [ Svg.Attributes.width (String.fromInt (sizePerCharacter * 27))
        , Svg.Attributes.height (String.fromInt height)
        ]
        (drawCircleRow 20 30 resultInputList ++ drawCircleRow (height - 20) 30 resultOutputList)


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
