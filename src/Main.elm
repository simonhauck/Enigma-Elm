module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Models.Enigma.EnigmaMachine as EnigmaMachine
import Models.Enigma.SubstitutionLog as Log
import Models.MessageHolder as MessageHolder
import Models.ServerMessageHolder as ServerMessageHolder
import View.ConfigurationView
import View.EncryptionView
import View.MessageHolderView
import View.StyleElements



-- TODO Rename HandleRandomCmd


type Msg
    = ConfigurationMsg View.ConfigurationView.ConfigurationMsg
    | MessageHolderMsg View.MessageHolderView.ServerMessageHolderMsg
    | EncryptionMsg View.EncryptionView.EncryptionMsg


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
            [ Html.h2 View.StyleElements.h2StyleElements [ Html.text "Preview" ]
            , View.EncryptionView.textInputBoxView model.messageHolder model.enigma.operationMode EncryptionMsg
            , View.MessageHolderView.displayEncryptionResult model.messageHolder MessageHolderMsg
            , View.EncryptionView.enigmaPreview model.enigma model.substitutionLog
            ]
        , Html.div
            []
            [ Html.h2 View.StyleElements.h2StyleElements [ Html.text "Server Messages" ]
            , View.MessageHolderView.displayServerMessages
                model.serverMessageHolder
                MessageHolderMsg
            ]
        ]



-- ---------------------------------------------------------------------------------------------------------------------
-- Browser functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Return the initial model of the mainView
-}
initialModel : ( Model, Cmd Msg )
initialModel =
    ( { enigma = EnigmaMachine.defaultEnigma
      , substitutionLog = Nothing
      , messageHolder = MessageHolder.defaultMessageHolder
      , serverMessageHolder = ServerMessageHolder.defaultServerMessageHolder
      }
      --      TODO Eta reduction somehow?
    , ServerMessageHolder.requestServerMessages (\response -> MessageHolderMsg (View.MessageHolderView.ResultLoadingServerMessages response))
    )


{-| Return the subscriptions for the given model
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    View.EncryptionView.subscription model.messageHolder model.enigma EncryptionMsg


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

        EncryptionMsg encryptionMsg ->
            let
                ( newEnigma, newMessageHolder, maybeLog ) =
                    View.EncryptionView.update encryptionMsg model.enigma model.messageHolder model.substitutionLog
            in
            ( { model | enigma = newEnigma, messageHolder = newMessageHolder, substitutionLog = maybeLog }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
