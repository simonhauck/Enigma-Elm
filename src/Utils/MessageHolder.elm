module Utils.MessageHolder exposing (MessageHolder, updateRawInput)

{-| -}


{-| Hold the string values for the raw and processed input and the out value
-}
type alias MessageHolder =
    { rawInput : String
    , prcessedInput : String
    , processedOutput : String
    }


updateRawInput : MessageHolder -> String -> MessageHolder
updateRawInput messageHolder newRawInput =
    { messageHolder | rawInput = newRawInput }
