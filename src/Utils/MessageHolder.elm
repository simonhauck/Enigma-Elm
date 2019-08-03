module Utils.MessageHolder exposing (MessageHolder, addProcessedChar, getFirstCharFromRawInput, updateRawInput)

import Maybe exposing (Maybe)


{-| Hold the string values for the raw and processed input and the out value
-}
type alias MessageHolder =
    { rawInput : String
    , processedInput : String
    , processedOutput : String
    }


{-| Update the raw input value of the messageHolder with the given string
-}
updateRawInput : MessageHolder -> String -> MessageHolder
updateRawInput messageHolder newRawInput =
    { messageHolder | rawInput = newRawInput }


{-| Get the first char of the rawInput of the messageHolder. The char will be removed from the rawInput.
If the rawInput is empty, Nothing will be returned as char
-}
getFirstCharFromRawInput : MessageHolder -> ( MessageHolder, Maybe Char )
getFirstCharFromRawInput messageHolder =
    let
        result =
            String.uncons messageHolder.rawInput
    in
    case result of
        Nothing ->
            ( messageHolder, Nothing )

        Just ( droppedChar, remainingString ) ->
            ( { messageHolder | rawInput = remainingString }, Just droppedChar )


addProcessedChar : MessageHolder -> Char -> Maybe Char -> MessageHolder
addProcessedChar messageHolder inputChar maybeOutputChar =
    case maybeOutputChar of
        Nothing ->
            messageHolder

        Just outputChar ->
            { messageHolder
                | processedInput = inputChar |> Char.toUpper |> String.fromChar |> String.append messageHolder.processedInput
                , processedOutput = outputChar |> Char.toUpper |> String.fromChar |> String.append messageHolder.processedOutput
            }
