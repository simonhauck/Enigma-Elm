module Models.MessageHolder exposing
    ( Config
    , EncryptionMode(..)
    , ForeignChar(..)
    , MessageHolder
    , addProcessedChar
    , copyConfig
    , defaultConfig
    , defaultMessageHolder
    , disableAutomaticEncryptionMode
    , getFirstCharFromRawInput
    , getFormattedProcessedInputOutput
    , getFormattedText
    , setDescription
    , setEncryptionSpeed
    , setProcessedInputAsRawInput
    , setRawInput
    , toggleEncryptionMode
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
    , config : Config
    }


type ForeignChar
    = Include
    | Ignore


type EncryptionMode
    = Automatic
    | Manual


type alias Config =
    { encryptionMode : EncryptionMode, encryptionSpeed : Int }



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| default Config
-}
defaultConfig : Config
defaultConfig =
    { encryptionMode = Manual, encryptionSpeed = 250 }


{-| Get a default message holder
-}
defaultMessageHolder : MessageHolder
defaultMessageHolder =
    { description = "", rawInput = "", processedInput = "", processedOutput = "", foreignCharOption = Include, config = defaultConfig }


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
            ( getFormattedText messageHolder messageHolder.processedInput
            , getFormattedText messageHolder messageHolder.processedOutput
            )

        Include ->
            ( messageHolder.processedInput, messageHolder.processedOutput )


{-| Get the text formatted for the given messageHolder
-}
getFormattedText : MessageHolder -> String -> String
getFormattedText messageHolder inputString =
    case messageHolder.foreignCharOption of
        Ignore ->
            Regex.replace (Maybe.withDefault Regex.never (Regex.fromString ".{5}")) (\match -> match.match ++ " ") inputString

        Include ->
            inputString


{-| Toggle the foreignCharOption between Ignore and Include
-}
toggleForeignCharOption : MessageHolder -> MessageHolder
toggleForeignCharOption messageHolder =
    case messageHolder.foreignCharOption of
        Include ->
            { messageHolder | foreignCharOption = Ignore }

        Ignore ->
            { messageHolder | foreignCharOption = Include }


{-| copy the config of the old MessageHolder to the new one and return the updated MessageHolder
-}
copyConfig : MessageHolder -> MessageHolder -> MessageHolder
copyConfig oldMessageHolder newMessageHolder =
    { newMessageHolder | config = oldMessageHolder.config }


{-| Set the encryption Speed in the config
-}
setEncryptionSpeed : MessageHolder -> Int -> MessageHolder
setEncryptionSpeed messageHolder newSpeedVal =
    let
        newConfig =
            { encryptionMode = messageHolder.config.encryptionMode, encryptionSpeed = newSpeedVal }
    in
    { messageHolder | config = newConfig }


{-| Toggle the encryptionMode in the config
-}
toggleEncryptionMode : MessageHolder -> MessageHolder
toggleEncryptionMode messageHolder =
    let
        newMode =
            case messageHolder.config.encryptionMode of
                Automatic ->
                    Manual

                Manual ->
                    Automatic
    in
    { messageHolder | config = { encryptionMode = newMode, encryptionSpeed = messageHolder.config.encryptionSpeed } }


{-| Disable the Automatic encryption
-}
disableAutomaticEncryptionMode : MessageHolder -> MessageHolder
disableAutomaticEncryptionMode messageHolder =
    { messageHolder | config = { encryptionMode = Manual, encryptionSpeed = messageHolder.config.encryptionSpeed } }


{-| Set the processed Input as rawInput. Set the processed Input/Output to ""
-}
setProcessedInputAsRawInput : MessageHolder -> MessageHolder
setProcessedInputAsRawInput messageHolder =
    { messageHolder
        | rawInput = messageHolder.processedInput ++ messageHolder.rawInput
        , processedInput = ""
        , processedOutput = ""
    }



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------
