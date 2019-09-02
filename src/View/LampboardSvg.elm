module View.LampboardSvg exposing (drawLampBoard)

import Html
import Models.Enigma.SubstitutionLog as Log
import Svg
import Svg.Attributes
import Utils.AlphabetHelper
import View.StyleElements



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Draw the lamp board with the given SubstitutionLog.
-}
drawLampBoard : Maybe Log.SubstitutionLog -> Html.Html msg
drawLampBoard maybeLog =
    let
        height =
            150

        width =
            500
    in
    Svg.svg
        [ Svg.Attributes.width <| String.fromInt width
        , Svg.Attributes.height <| String.fromInt height
        ]
        (drawLampBoardRows lampBoardKeyOrder maybeLog 50 20)



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Space between two rows of lamps
-}
spaceYBetweenLamps : Int
spaceYBetweenLamps =
    50


{-| Space between two lamps
-}
spaceXBetweenLamps : Int
spaceXBetweenLamps =
    50


{-| Offset for every second row
-}
xOffsetLength : Int
xOffsetLength =
    toFloat spaceXBetweenLamps / 2 |> round


{-| Character indices for the keys in the lampBoard. These are from the official enigma.
See <https://upload.wikimedia.org/wikipedia/commons/5/5a/Enigma_beschriftet_cropped.jpg>
-}
lampBoardKeyOrder : List (List Int)
lampBoardKeyOrder =
    [ [ 16, 22, 4, 17, 19, 25, 20, 8, 14 ]
    , [ 0, 18, 3, 5, 6, 7, 9, 10 ]
    , [ 15, 24, 23, 2, 21, 1, 13, 12, 11 ]
    ]


{-| Draw the given lamp rows and highlight the active lamp
rowList - a list with the rows on the plugboard
maybeLog - the log that will be displayed
x - the xCoordinate in the svg
y - the yCoordinate in the svg
-}
drawLampBoardRows : List (List Int) -> Maybe Log.SubstitutionLog -> Int -> Int -> List (Svg.Svg msg)
drawLampBoardRows rowList maybeLog x y =
    let
        calculateOffsetFunction =
            \index -> modBy 2 index |> (*) xOffsetLength

        calculateYFunction =
            \index -> index * spaceYBetweenLamps + y
    in
    List.indexedMap
        (\index row ->
            drawLampBoardRow row maybeLog (calculateOffsetFunction index) x (calculateYFunction index)
        )
        rowList
        |> List.concat


{-| Draw a row with lamps for the given character list. Highlight the lamp if its the result of the substitution log.
characterList - a list with the indices of the characters for the row
maybeLog - the substitutionLog with the last character
xOffset - the offset for every second row
x - the xCoordinate of the row
y - the yCoordinate of the row
-}
drawLampBoardRow : List Int -> Maybe Log.SubstitutionLog -> Int -> Int -> Int -> List (Svg.Svg msg)
drawLampBoardRow characterList maybeLog xOffset x y =
    let
        calculateCoordinateFunction =
            \index -> spaceXBetweenLamps * index + xOffset + x

        getCharFunction =
            \characterIndex -> Just characterIndex |> Utils.AlphabetHelper.characterIndexToCharacter |> Maybe.withDefault '-'

        -- Activate the lamp if the maybeLog is Just log and the resultChar is the same as the given index
        activeLampFunction =
            \characterIndex -> (Maybe.map (\log -> characterIndex == Log.getResultChar log) >> Maybe.withDefault False) maybeLog
    in
    List.indexedMap
        (\index charIndex ->
            getCharFunction charIndex |> drawCharacterCircle (calculateCoordinateFunction index) y (activeLampFunction charIndex)
        )
        characterList
        |> List.concat


{-| Draw a circle at the given coordinates with a character inside
-}
drawCharacterCircle : Int -> Int -> Bool -> Char -> List (Svg.Svg msg)
drawCharacterCircle x y fillCircle character =
    let
        characterYOffset =
            5
    in
    [ Svg.circle
        ([ Svg.Attributes.cx (String.fromInt x)
         , Svg.Attributes.cy (String.fromInt y)
         , Svg.Attributes.r "15"
         , Svg.Attributes.stroke "black"
         , Svg.Attributes.strokeWidth "2"
         ]
            ++ (if fillCircle then
                    [ Svg.Attributes.fill "#fbff2e" ]

                else
                    [ Svg.Attributes.fill View.StyleElements.thirdColor
                    , Svg.Attributes.opacity "0.5"
                    ]
               )
        )
        []
    , Svg.text_
        [ Svg.Attributes.x <| String.fromInt x
        , Svg.Attributes.y <| String.fromInt <| y + characterYOffset
        , Svg.Attributes.textAnchor "middle"
        , Svg.Attributes.fontFamily "monospace"
        , Svg.Attributes.fontSize "16px"
        ]
        [ Svg.text <| String.fromChar character ]
    ]
