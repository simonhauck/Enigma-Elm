module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Models.Enigma.EnigmaMachine as EnigmaMachine
import Models.Enigma.OperationMode
import Models.Enigma.SubstitutionLog as Log
import Models.MessageHolder as MessageHolder
import Models.ServerMessageHolder as ServerMessageHolder
import String
import Time
import View.ConfigurationView
import View.EnigmaSvg
import View.MessageHolderView
import View.StyleElements



-- TODO Rename HandleRandomCmd


type Msg
    = ConfigurationMsg View.ConfigurationView.ConfigurationMsg
    | MessageHolderMsg View.MessageHolderView.ServerMessageHolderMsg
    | EncryptCharTick
    | UpdateDescription String


type alias Model =
    { enigma : EnigmaMachine.Enigma
    , substitutionLog : Maybe Log.SubstitutionLog
    , messageHolder : MessageHolder.MessageHolder
    , serverMessageHolder : ServerMessageHolder.ServerMessageHolder
    }



-- ---------------------------------------------------------------------------------------------------------------------
-- View
-- ---------------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    Html.div
        [ View.StyleElements.backgroundImage ]
        [ View.ConfigurationView.displayEnigmaConfiguration
            model.enigma
            model.messageHolder
            ConfigurationMsg
        , Html.div
            []
            [ Html.h2 [ Html.Attributes.align "center" ] [ Html.text "Preview" ]
            , encryptionView model
            ]
        , Html.div
            []
            [ Html.h2 [ Html.Attributes.align "center" ] [ Html.text "Server Messages" ]
            , View.MessageHolderView.displayServerMessages
                model.serverMessageHolder
                MessageHolderMsg
            ]
        ]



-- ---------------------------------------------------------------------------------------------------------------------
-- Encryption View
-- ---------------------------------------------------------------------------------------------------------------------


encryptionView : Model -> Html Msg
encryptionView model =
    Html.div
        []
        [ Html.div
            []
            [ Html.h3 [] [ Html.text "Encryption Results" ]
            , encryptionResultView model
            ]
        , View.MessageHolderView.textInputBoxView model.messageHolder model.enigma.operationMode MessageHolderMsg
        , Html.div
            []
            [ Html.h3 [] [ Html.text "Enigma View" ]
            , enigmaSvg model
            ]
        ]


encryptionResultView : Model -> Html Msg
encryptionResultView model =
    let
        ( formattedInput, formattedOutput ) =
            MessageHolder.getFormattedProcessedInputOutput model.messageHolder
    in
    Html.div
        []
        [ Html.table
            []
            [ Html.tr
                []
                [ Html.td [] [ Html.text "Processed Input: " ]
                , Html.td [] [ Html.text formattedInput ]
                ]
            , Html.tr
                []
                [ Html.td [] [ Html.text "Processed Output: " ]
                , Html.td [] [ Html.text formattedOutput ]
                ]
            ]
        , Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.placeholder "Add a description"
            , Html.Attributes.value model.messageHolder.description
            , Html.Events.onInput UpdateDescription
            ]
            []
        , Html.button
            [ Html.Events.onClick (MessageHolderMsg View.MessageHolderView.SendMessageToServer)
            , Html.Attributes.disabled (String.isEmpty model.messageHolder.description || String.isEmpty model.messageHolder.processedOutput)
            ]
            [ Html.text "Send message to server" ]
        ]


enigmaSvg : Model -> Html Msg
enigmaSvg model =
    View.EnigmaSvg.enigmaSvg model.enigma model.substitutionLog


enableAttributeWhenInEncryption : Model -> Html.Attribute Msg
enableAttributeWhenInEncryption model =
    case model.enigma.operationMode of
        Models.Enigma.OperationMode.Encryption ->
            Html.Attributes.disabled False

        Models.Enigma.OperationMode.Configuration ->
            Html.Attributes.disabled True



-- ---------------------------------------------------------------------------------------------------------------------
-- Util Functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Substitute a maybeChar with the given model.
The state of the enigma will be updated and the inputChar/resultChar will be added
-}
substituteChar : Model -> Maybe Char -> Model
substituteChar model maybeInputChar =
    let
        inputChar =
            Maybe.withDefault '-' maybeInputChar

        ( newEnigma, maybeSubstitutionLog, maybeOutputChar ) =
            EnigmaMachine.performRotationAndSubstitution model.enigma inputChar

        updatedMessageHolder =
            MessageHolder.addProcessedChar model.messageHolder inputChar maybeOutputChar
    in
    { model | enigma = newEnigma, messageHolder = updatedMessageHolder, substitutionLog = maybeSubstitutionLog }



-- ---------------------------------------------------------------------------------------------------------------------
-- Browser functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Return the initial model of the mainView
-}
initialModel : ( Model, Cmd Msg )
initialModel =
    let
        enigma =
            EnigmaMachine.defaultEnigma
    in
    ( { enigma = enigma
      , substitutionLog = Nothing
      , messageHolder = MessageHolder.defaultMessageHolder
      , serverMessageHolder = ServerMessageHolder.defaultServerMessageHolder
      }
      --      TODO Eta reduction somwhow?
    , ServerMessageHolder.requestServerMessages (\reponse -> MessageHolderMsg (View.MessageHolderView.ResultLoadingServerMessages reponse))
    )


{-| Return the subscriptions for the given model
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case ( model.enigma.operationMode, model.messageHolder.config.encryptionMode ) of
        ( Models.Enigma.OperationMode.Encryption, MessageHolder.Automatic ) ->
            if String.isEmpty model.messageHolder.rawInput then
                Sub.none

            else
                Time.every (toFloat model.messageHolder.config.encryptionSpeed) (\_ -> EncryptCharTick)

        _ ->
            Sub.none


{-| Update the given model with the given msg
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConfigurationMsg configurationMessage ->
            let
                ( newEnigma, newMessageHolder, newCmd ) =
                    View.ConfigurationView.update configurationMessage model.enigma model.messageHolder ConfigurationMsg
            in
            ( { model | enigma = newEnigma, messageHolder = newMessageHolder }, newCmd )

        MessageHolderMsg serverMessageHolderMessage ->
            let
                ( newServerMessageHolder, newMessageHolder, newCmd ) =
                    View.MessageHolderView.update serverMessageHolderMessage model.serverMessageHolder model.messageHolder MessageHolderMsg
            in
            ( { model | serverMessageHolder = newServerMessageHolder, messageHolder = newMessageHolder }, newCmd )

        UpdateDescription input ->
            ( { model | messageHolder = MessageHolder.setDescription model.messageHolder input }, Cmd.none )

        EncryptCharTick ->
            let
                ( updatedMessageHolder, maybeInputChar ) =
                    MessageHolder.getFirstCharFromRawInput model.messageHolder

                updatedModel =
                    { model | messageHolder = updatedMessageHolder }
            in
            ( substituteChar updatedModel maybeInputChar, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
