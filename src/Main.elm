module Main exposing (main)

import Browser
import Enigma.EnigmaMachine exposing (Enigma)
import Enigma.SubstitutionLog
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import String
import Time
import Utils.MessageHolder exposing (ForeignChar, MessageHolder)
import Utils.ServerMessageHolder
import View.ConfigurationView
import View.EnigmaSvg
import View.ServerMessageView
import View.StyleElements



-- TODO Rename HandleRandomCmd


type Msg
    = ConfigurationMsg View.ConfigurationView.ConfigurationMsg
    | EncryptCharTick
    | UpdateRawInput String
    | UpdateDescription String
    | ToggleEncryptionMode
    | SetEncryptionModeSpeed Int
    | LoadServerMessages
    | SelectServerMessage MessageHolder
    | SendMessageToServer
    | ReceiveServerMessageHolder (Result Http.Error (List MessageHolder))


type EncryptionMode
    = Automatic
    | Manual


type alias TextInputConfig =
    { encryptionMode : EncryptionMode, encryptionSpeed : Int }


type alias Model =
    { enigma : Enigma
    , substitutionLog : Maybe Enigma.SubstitutionLog.SubstitutionLog
    , messageHolder : MessageHolder
    , textInputConfig : TextInputConfig
    , serverMessageHolder : Utils.ServerMessageHolder.ServerMessageHolder
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
            , View.ServerMessageView.displayServerMessages SelectServerMessage LoadServerMessages model.serverMessageHolder
            ]
        ]



-- ---------------------------------------------------------------------------------------------------------------------
-- Configuration View
-- ---------------------------------------------------------------------------------------------------------------------
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
        , textInputView model
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
            Utils.MessageHolder.getFormattedProcessedInputOutput model.messageHolder
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
            [ Html.Events.onClick SendMessageToServer
            , Html.Attributes.disabled (String.isEmpty model.messageHolder.description || String.isEmpty model.messageHolder.processedOutput)
            ]
            [ Html.text "Send message to server" ]
        ]


textInputView : Model -> Html Msg
textInputView model =
    Html.div []
        [ Html.h3 [] [ Html.text "Text Input" ]
        , textInputField model
        , textInputToggleButton model
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "25"
                , Html.Attributes.max "1000"
                , Html.Attributes.value (String.fromInt model.textInputConfig.encryptionSpeed)
                , Html.Attributes.step "25"
                , Html.Events.onInput (\val -> SetEncryptionModeSpeed (Maybe.withDefault 250 (String.toInt val)))
                , enableAttributeWhenInEncryption model
                ]
                []
            , Html.text ("Time between Ticks: " ++ String.fromInt model.textInputConfig.encryptionSpeed)
            ]
        ]


enigmaSvg : Model -> Html Msg
enigmaSvg model =
    View.EnigmaSvg.enigmaSvg model.enigma model.substitutionLog


textInputField : Model -> Html Msg
textInputField model =
    Html.textarea
        [ Html.Attributes.placeholder "Enter your text here"
        , Html.Attributes.value model.messageHolder.rawInput
        , Html.Events.onInput UpdateRawInput
        ]
        []


textInputToggleButton : Model -> Html Msg
textInputToggleButton model =
    Html.button
        [ Html.Events.onClick ToggleEncryptionMode
        , enableAttributeWhenInEncryption model
        ]
        [ case model.textInputConfig.encryptionMode of
            Automatic ->
                Html.text "Disable automatic encryption"

            Manual ->
                Html.text "Enable automatic encryption"
        ]


enableAttributeWhenInEncryption : Model -> Html.Attribute Msg
enableAttributeWhenInEncryption model =
    case model.enigma.operationMode of
        Enigma.EnigmaMachine.Encryption ->
            Html.Attributes.disabled False

        Enigma.EnigmaMachine.Configuration ->
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
            Enigma.EnigmaMachine.performRotationAndSubstitution model.enigma inputChar

        updatedMessageHolder =
            Utils.MessageHolder.addProcessedChar model.messageHolder inputChar maybeOutputChar
    in
    { model | enigma = newEnigma, messageHolder = updatedMessageHolder, substitutionLog = maybeSubstitutionLog }


disableAutomaticTextInput : TextInputConfig -> TextInputConfig
disableAutomaticTextInput textInputConfig =
    { textInputConfig | encryptionMode = Manual }



-- ---------------------------------------------------------------------------------------------------------------------
-- Browser functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Return the initial model of the mainView
-}
initialModel : ( Model, Cmd Msg )
initialModel =
    let
        enigma =
            Enigma.EnigmaMachine.defaultEnigma

        textInputConfig =
            { encryptionMode = Manual, encryptionSpeed = 250 }
    in
    ( { enigma = enigma
      , substitutionLog = Nothing
      , messageHolder = Utils.MessageHolder.defaultMessageHolder
      , textInputConfig = textInputConfig
      , serverMessageHolder = Utils.ServerMessageHolder.defaultServerMessageHolder
      }
    , Utils.ServerMessageHolder.requestServerMessages ReceiveServerMessageHolder
    )


{-| Return the subscriptions for the given model
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case ( model.enigma.operationMode, model.textInputConfig.encryptionMode ) of
        ( Enigma.EnigmaMachine.Encryption, Automatic ) ->
            if String.isEmpty model.messageHolder.rawInput then
                Sub.none

            else
                Time.every (toFloat model.textInputConfig.encryptionSpeed) (\_ -> EncryptCharTick)

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

        UpdateRawInput input ->
            ( { model | messageHolder = Utils.MessageHolder.setRawInput model.messageHolder input }, Cmd.none )

        UpdateDescription input ->
            ( { model | messageHolder = Utils.MessageHolder.setDescription model.messageHolder input }, Cmd.none )

        ToggleEncryptionMode ->
            let
                textInputConfig =
                    model.textInputConfig

                newMode =
                    case textInputConfig.encryptionMode of
                        Automatic ->
                            Manual

                        Manual ->
                            Automatic
            in
            ( { model | textInputConfig = { textInputConfig | encryptionMode = newMode } }, Cmd.none )

        SetEncryptionModeSpeed newSpeedVal ->
            let
                textInputConfig =
                    model.textInputConfig
            in
            ( { model | textInputConfig = { textInputConfig | encryptionSpeed = newSpeedVal } }, Cmd.none )

        EncryptCharTick ->
            let
                ( updatedMessageHolder, maybeInputChar ) =
                    Utils.MessageHolder.getFirstCharFromRawInput model.messageHolder

                updatedModel =
                    { model | messageHolder = updatedMessageHolder }
            in
            ( substituteChar updatedModel maybeInputChar, Cmd.none )

        LoadServerMessages ->
            ( { model | serverMessageHolder = Utils.ServerMessageHolder.Loading }
            , Utils.ServerMessageHolder.requestServerMessages ReceiveServerMessageHolder
            )

        SelectServerMessage messageHolder ->
            ( { model | messageHolder = messageHolder, textInputConfig = disableAutomaticTextInput model.textInputConfig }, Cmd.none )

        SendMessageToServer ->
            ( { model | messageHolder = Utils.MessageHolder.defaultMessageHolder }
            , Utils.ServerMessageHolder.sendMessageToServer model.messageHolder ReceiveServerMessageHolder
            )

        ReceiveServerMessageHolder result ->
            ( { model | serverMessageHolder = Utils.ServerMessageHolder.handleServerResponse result }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
