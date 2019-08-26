module Utils.MessageHolder exposing
    ( ForeignChar(..)
    , MessageHolder
    , addProcessedChar
    , defaultMessageHolder
    , getFirstCharFromRawInput
    , getFormattedProcessedInputOutput
    , setDescription
    , setRawInput
    , toggleForeignCharOption
    )

import Maybe exposing (Maybe)
import Regex


{-| Hold the string values for the raw and processed input and the out value
-}
type alias MessageHolder =
    { description : String
    , rawInput : String
    , processedInput : String
    , processedOutput : String
    , foreignCharOption : ForeignChar
    }


type ForeignChar
    = Include
    | Ignore



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Get a default message holder
-}
defaultMessageHolder : MessageHolder
defaultMessageHolder =
    { description = "", rawInput = "", processedInput = "", processedOutput = "", foreignCharOption = Include }


{-| Set the raw input value of the messageHolder with the given string
-}
setRawInput : MessageHolder -> String -> MessageHolder
setRawInput messageHolder newRawInput =
    { messageHolder | rawInput = newRawInput }


{-| Set the value for the description
-}
setDescription : MessageHolder -> String -> MessageHolder
setDescription messageHolder newInput =
    { messageHolder | description = newInput }


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


{-| Add a processed char to the messageHolder.
If the option ForeignChar is set to Ignore the value will be ignored if the maybeOutputChar is Nothing
Else it will add the value to the MessageHolder.
-}
addProcessedChar : MessageHolder -> Char -> Maybe Char -> MessageHolder
addProcessedChar messageHolder inputChar maybeOutputChar =
    case ( messageHolder.foreignCharOption, maybeOutputChar ) of
        ( Ignore, Nothing ) ->
            messageHolder

        ( _, _ ) ->
            { messageHolder
                | processedInput = inputChar |> Char.toUpper |> String.fromChar |> String.append messageHolder.processedInput
                , processedOutput =
                    maybeOutputChar
                        |> Maybe.withDefault inputChar
                        |> Char.toUpper
                        |> String.fromChar
                        |> String.append messageHolder.processedOutput
            }


{-| Return the processed input/output formatted corresponding to the ForeignCharOption
-}
getFormattedProcessedInputOutput : MessageHolder -> ( String, String )
getFormattedProcessedInputOutput messageHolder =
    case messageHolder.foreignCharOption of
        Ignore ->
            ( Regex.replace (Maybe.withDefault Regex.never (Regex.fromString ".{5}")) (\match -> match.match ++ " ") messageHolder.processedInput
            , Regex.replace (Maybe.withDefault Regex.never (Regex.fromString ".{5}")) (\match -> match.match ++ " ") messageHolder.processedOutput
            )

        Include ->
            ( messageHolder.processedInput, messageHolder.processedOutput )


{-| Toggle the foreignCharOption between Ignore and Include
-}
toggleForeignCharOption : MessageHolder -> MessageHolder
toggleForeignCharOption messageHolder =
    case messageHolder.foreignCharOption of
        Include ->
            { messageHolder | foreignCharOption = Ignore }

        Ignore ->
            { messageHolder | foreignCharOption = Include }



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------
