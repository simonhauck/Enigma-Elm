module View.EncryptionView exposing (EncryptionMsg(..), enigmaPreview, subscription, textInputBoxView, update)

import Flip
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Models.Enigma.EnigmaMachine as EnigmaMachine
import Models.Enigma.OperationMode as OperationMode
import Models.Enigma.SubstitutionLog as Log
import Models.MessageHolder as MessageHolder
import Time
import View.EnigmaSvg
import View.StyleElements


type alias ConvertEncryptionMsg msg =
    EncryptionMsg -> msg


type EncryptionMsg
    = EncryptChar
    | SetMessageHolder MessageHolder.MessageHolder



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


textInputBoxView : MessageHolder.MessageHolder -> OperationMode.OperationMode -> ConvertEncryptionMsg msg -> Html msg
textInputBoxView messageHolder operationMode convertEncryptionMsg =
    Html.div
        []
        [ Html.h3 View.StyleElements.h3StyleElements [ Html.text "Text Input" ]
        , Html.textarea
            ([ Html.Attributes.placeholder "Enter your text here"
             , Html.Attributes.value messageHolder.rawInput
             , Html.Events.onInput (\val -> MessageHolder.setRawInput messageHolder val |> SetMessageHolder |> convertEncryptionMsg)
             ]
                ++ View.StyleElements.textarea
            )
            []
        , Html.button
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
        , Html.button
            ((Html.Events.onClick <| convertEncryptionMsg <| EncryptChar)
                :: (if String.isEmpty messageHolder.rawInput then
                        Html.Attributes.disabled True

                    else
                        enableAttributeWhenInEncryption operationMode
                   )
                :: View.StyleElements.buttonStyleElements
            )
            [ Html.text "Single Step" ]
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "25"
                , Html.Attributes.max "1000"
                , Html.Attributes.value <| String.fromInt messageHolder.config.encryptionSpeed
                , Html.Attributes.step "25"
                , Html.Events.onInput
                    (\val ->
                        String.toInt val
                            |> Maybe.withDefault 250
                            |> MessageHolder.setEncryptionSpeed messageHolder
                            |> SetMessageHolder
                            |> convertEncryptionMsg
                    )
                , enableAttributeWhenInEncryption operationMode
                ]
                []
            , Html.text ("Time between Ticks: " ++ String.fromInt messageHolder.config.encryptionSpeed)
            ]
        ]


enigmaPreview : EnigmaMachine.Enigma -> Maybe Log.SubstitutionLog -> Html msg
enigmaPreview enigma maybeLog =
    Html.div
        []
        [ Html.div
            []
            [ Html.h3 View.StyleElements.h3StyleElements [ Html.text "Enigma View" ]
            , View.EnigmaSvg.enigmaSvg enigma maybeLog
            ]
        ]


update : EncryptionMsg -> EnigmaMachine.Enigma -> MessageHolder.MessageHolder -> Maybe Log.SubstitutionLog -> ( EnigmaMachine.Enigma, MessageHolder.MessageHolder, Maybe Log.SubstitutionLog )
update encryptionMsg enigma messageHolder maybeLog =
    case encryptionMsg of
        EncryptChar ->
            substituteCharWithMessageHolder enigma messageHolder

        SetMessageHolder newMessageHolder ->
            ( enigma, newMessageHolder, maybeLog )


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


enableAttributeWhenInEncryption : OperationMode.OperationMode -> Html.Attribute msg
enableAttributeWhenInEncryption operationMode =
    case operationMode of
        OperationMode.Configuration ->
            Html.Attributes.disabled True

        OperationMode.Encryption ->
            Html.Attributes.disabled False
