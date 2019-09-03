module View.EncryptionView exposing (EncryptionMsg(..), enigmaPreview, lampBoardPreview, subscription, textInputBoxView, update)

import Flip
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Model exposing (Model)
import Models.Enigma.EnigmaMachine as EnigmaMachine
import Models.Enigma.OperationMode as OperationMode
import Models.Enigma.SubstitutionLog as Log
import Models.MessageHolder as MessageHolder
import Time
import View.EnigmaSvg
import View.HtmlComponents
import View.LampboardSvg
import View.StyleElements


type alias ConvertEncryptionMsg msg =
    EncryptionMsg -> msg


type EncryptionMsg
    = EncryptChar
    | EncryptAll
    | SetMessageHolder MessageHolder.MessageHolder



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


textInputBoxView : MessageHolder.MessageHolder -> OperationMode.OperationMode -> ConvertEncryptionMsg msg -> Html msg
textInputBoxView messageHolder operationMode convertEncryptionMsg =
    Html.div
        ([ View.StyleElements.smallMargin ] ++ View.StyleElements.smallElementBox)
        [ Html.h3 View.StyleElements.h3StyleElements [ Html.text "Text Input" ]
        , Html.div
            [ Html.Attributes.style "width" "100%" ]
            [ textInputArea messageHolder convertEncryptionMsg
            , encryptionSpeedSlider messageHolder operationMode convertEncryptionMsg
            ]
        , Html.div
            []
            [ toggleAutomaticEncryptionButton messageHolder operationMode convertEncryptionMsg
            , singleEncryptionStepButton messageHolder operationMode convertEncryptionMsg
            , instantEncryptionButton messageHolder operationMode convertEncryptionMsg
            ]
        ]


enigmaPreview : EnigmaMachine.Enigma -> Maybe Log.SubstitutionLog -> Html msg
enigmaPreview enigma maybeLog =
    Html.div
        ([ View.StyleElements.flexDirectionColumn, View.StyleElements.smallMargin ] ++ View.StyleElements.smallElementBox)
        [ Html.div
            []
            [ Html.h3 View.StyleElements.h3StyleElements [ Html.text "Enigma View" ]
            , View.EnigmaSvg.enigmaSvg enigma maybeLog
            ]
        ]


lampBoardPreview : Maybe Log.SubstitutionLog -> Html msg
lampBoardPreview maybeLog =
    Html.div
        ([ View.StyleElements.smallMargin ] ++ View.StyleElements.smallElementBox)
        [ Html.h3 View.StyleElements.h3StyleElements [ Html.text "Lamps" ]
        , View.LampboardSvg.drawLampBoard maybeLog
        ]


update : EncryptionMsg -> Model -> ( Model, Cmd msg )
update encryptionMsg model =
    case encryptionMsg of
        EncryptChar ->
            let
                ( enigma, messageHolder, maybeLog ) =
                    substituteCharWithMessageHolder model.enigma model.messageHolder
            in
            ( { model | enigma = enigma, messageHolder = messageHolder, substitutionLog = maybeLog }, Cmd.none )

        EncryptAll ->
            let
                ( enigma, messageHolder, maybeLog ) =
                    substituteRawInput model.enigma model.messageHolder
            in
            ( { model | enigma = enigma, messageHolder = messageHolder, substitutionLog = maybeLog }, Cmd.none )

        SetMessageHolder newMessageHolder ->
            ( { model | messageHolder = newMessageHolder }, Cmd.none )


{-| Get the subscriptions to encrypt the raw input
-}
subscription : MessageHolder.MessageHolder -> EnigmaMachine.Enigma -> ConvertEncryptionMsg msg -> Sub msg
subscription messageHolder enigma convertMessageHolderMsg =
    case ( messageHolder.config.encryptionMode, enigma.operationMode ) of
        ( MessageHolder.Automatic, OperationMode.Encryption ) ->
            if String.isEmpty messageHolder.rawInput then
                Sub.none

            else
                toFloat messageHolder.config.encryptionSpeed |> Flip.flip Time.every (\_ -> convertMessageHolderMsg EncryptChar)

        _ ->
            Sub.none



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


textInputArea : MessageHolder.MessageHolder -> ConvertEncryptionMsg msg -> Html msg
textInputArea messageHolder convertEncryptionMsg =
    Html.textarea
        ([ Html.Attributes.placeholder "Enter your text here..."
         , Html.Attributes.value messageHolder.rawInput
         , Html.Events.onInput (\val -> MessageHolder.setRawInput messageHolder val |> SetMessageHolder |> convertEncryptionMsg)
         ]
            ++ View.StyleElements.textarea
        )
        []


encryptionSpeedSlider : MessageHolder.MessageHolder -> OperationMode.OperationMode -> ConvertEncryptionMsg msg -> Html msg
encryptionSpeedSlider messageHolder operationMode convertEncryptionMsg =
    Html.div
        [ View.StyleElements.flexDisplay ]
        [ Html.div
            [ Html.Attributes.style "width" "150"
            , View.StyleElements.fontColor
            , View.StyleElements.fontFamily
            , View.StyleElements.fontSizeText
            , View.StyleElements.smallMargin
            ]
            [ Html.text "Encryption Speed: " ]
        , View.HtmlComponents.rangeSlider
            [ Html.Attributes.min "25"
            , Html.Attributes.max "1000"
            , Html.Attributes.value <| String.fromInt messageHolder.config.encryptionSpeed
            , Html.Attributes.step "5"
            , Html.Attributes.style "direction" "rtl"
            , enableAttributeWhenInEncrptionAndManuelMode operationMode messageHolder.config
            , View.StyleElements.smallMargin
            , View.StyleElements.flexGrow
            , Html.Events.onInput
                (\val ->
                    String.toInt val
                        |> Maybe.withDefault 250
                        |> MessageHolder.setEncryptionSpeed messageHolder
                        |> SetMessageHolder
                        |> convertEncryptionMsg
                )
            ]
            []
        ]


toggleAutomaticEncryptionButton : MessageHolder.MessageHolder -> OperationMode.OperationMode -> ConvertEncryptionMsg msg -> Html msg
toggleAutomaticEncryptionButton messageHolder operationMode convertEncryptionMsg =
    Html.button
        ((Html.Events.onClick <| convertEncryptionMsg <| SetMessageHolder <| MessageHolder.toggleEncryptionMode messageHolder)
            :: enableAttributeWhenInEncryption operationMode
            :: View.StyleElements.buttonStyleElements
        )
        [ case messageHolder.config.encryptionMode of
            MessageHolder.Automatic ->
                Html.text "Disable automatic encryption"

            MessageHolder.Manual ->
                Html.text "Enable automatic encryption"
        ]


singleEncryptionStepButton : MessageHolder.MessageHolder -> OperationMode.OperationMode -> ConvertEncryptionMsg msg -> Html msg
singleEncryptionStepButton messageHolder operationMode convertEncryptionMsg =
    Html.button
        ((Html.Events.onClick <| convertEncryptionMsg <| EncryptChar)
            :: enableEncryptionStepButton messageHolder operationMode
            :: View.StyleElements.buttonStyleElements
        )
        [ Html.text "Single Step" ]


instantEncryptionButton : MessageHolder.MessageHolder -> OperationMode.OperationMode -> ConvertEncryptionMsg msg -> Html msg
instantEncryptionButton messageHolder operationMode convertEncryptionMsg =
    Html.button
        ((Html.Events.onClick <| convertEncryptionMsg <| EncryptAll)
            :: enableEncryptionStepButton messageHolder operationMode
            :: View.StyleElements.buttonStyleElements
        )
        [ Html.text "Instant Encryption" ]


{-| Substitute the first character from the rawInput in the messageHolder with the given enigma and add the result
to the messageHolder processedInput/Output.
The result contains the updated/rotated enigma, the updated messageHolder and the maybe SubstitutionLog for the character
-}
substituteCharWithMessageHolder : EnigmaMachine.Enigma -> MessageHolder.MessageHolder -> ( EnigmaMachine.Enigma, MessageHolder.MessageHolder, Maybe Log.SubstitutionLog )
substituteCharWithMessageHolder enigma messageHolder =
    let
        ( updatedMessageHolder, maybeInputChar ) =
            MessageHolder.getFirstCharFromRawInput messageHolder
    in
    substituteChar enigma updatedMessageHolder maybeInputChar


{-| Substitute a maybeChar with the given enigma and substitutionHolder.
The state of the enigma will be updated and the inputChar/resultChar will be added
-}
substituteChar : EnigmaMachine.Enigma -> MessageHolder.MessageHolder -> Maybe Char -> ( EnigmaMachine.Enigma, MessageHolder.MessageHolder, Maybe Log.SubstitutionLog )
substituteChar enigma messageHolder maybeInputChar =
    let
        inputChar =
            Maybe.withDefault '-' maybeInputChar

        ( newEnigma, maybeSubstitutionLog, maybeOutputChar ) =
            EnigmaMachine.performRotationAndSubstitution enigma inputChar

        updatedMessageHolder =
            MessageHolder.addProcessedChar messageHolder inputChar maybeOutputChar
    in
    ( newEnigma, updatedMessageHolder, maybeSubstitutionLog )


{-| Substitute the complete raw input in the given messageHolder. The state of the enigma will be updated after each
character. The result contains the final enigma, the messageHolder and the maybe SubstitutionLog of the last character
-}
substituteRawInput : EnigmaMachine.Enigma -> MessageHolder.MessageHolder -> ( EnigmaMachine.Enigma, MessageHolder.MessageHolder, Maybe Log.SubstitutionLog )
substituteRawInput startEnigma startMessageHolder =
    let
        emptyRawInputMessageHolder =
            { startMessageHolder | rawInput = "" }
    in
    String.foldl
        (\inputChar ( enigma, messageHolder, _ ) ->
            substituteChar enigma messageHolder (Just inputChar)
        )
        ( startEnigma, emptyRawInputMessageHolder, Nothing )
        startMessageHolder.rawInput


{-| The Html disabled attribute. The component will be disabled when the OperationMode is Configuration
-}
enableAttributeWhenInEncryption : OperationMode.OperationMode -> Html.Attribute msg
enableAttributeWhenInEncryption operationMode =
    case operationMode of
        OperationMode.Configuration ->
            Html.Attributes.disabled True

        OperationMode.Encryption ->
            Html.Attributes.disabled False


enableAttributeWhenInEncrptionAndManuelMode : OperationMode.OperationMode -> MessageHolder.Config -> Html.Attribute msg
enableAttributeWhenInEncrptionAndManuelMode operationMode config =
    case ( operationMode, config.encryptionMode ) of
        ( OperationMode.Encryption, MessageHolder.Manual ) ->
            Html.Attributes.disabled False

        ( _, _ ) ->
            Html.Attributes.disabled True


{-| The attribute is enabled when the OperationMode is Encryption and the rawInput of the messageHolder is not empty
-}
enableEncryptionStepButton : MessageHolder.MessageHolder -> OperationMode.OperationMode -> Html.Attribute msg
enableEncryptionStepButton messageHolder operationMode =
    if String.isEmpty messageHolder.rawInput then
        Html.Attributes.disabled True

    else
        enableAttributeWhenInEncryption operationMode
