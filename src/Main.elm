module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Model exposing (Model)
import Models.Enigma.EnigmaMachine
import Models.ServerMessageHolder as ServerMessageHolder
import View.ConfigurationView
import View.EncryptionView
import View.MessageHolderView
import View.StyleElements


type Msg
    = ConfigurationMsg View.ConfigurationView.ConfigurationMsg
    | MessageHolderMsg View.MessageHolderView.ServerMessageHolderMsg
    | EncryptionMsg View.EncryptionView.EncryptionMsg



-- ---------------------------------------------------------------------------------------------------------------------
-- View
-- ---------------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    Html.div
        View.StyleElements.backgroundImage
        [ Html.div
            [ Html.Attributes.style "max-width" "1920px"
            , Html.Attributes.style "width" "fit-content"
            ]
            [ Html.h1 View.StyleElements.h1StyleElements [ Html.text "Elmigma" ]
            , Html.div
                [ View.StyleElements.flexDisplay ]
                [ Html.div
                    [ View.StyleElements.mediumMargin ]
                    [ View.ConfigurationView.displayEnigmaConfiguration
                        model.enigma
                        model.messageHolder
                        ConfigurationMsg
                    ]
                , Html.div
                    [ Html.Attributes.style "width" "-webkit-fill-available"
                    , View.StyleElements.mediumMargin
                    ]
                    [ View.MessageHolderView.displayServerMessages
                        model.serverMessageHolder
                        MessageHolderMsg
                    ]
                ]
            , Html.div
                []
                [ Html.h2 View.StyleElements.h2StyleElements [ Html.text "Preview" ]
                , Html.div
                    [ View.StyleElements.mediumMargin ]
                    [ View.MessageHolderView.displayEncryptionResult model.messageHolder MessageHolderMsg ]
                , Html.div
                    [ View.StyleElements.flexDisplay ]
                    [ Html.div
                        [ View.StyleElements.mediumMargin ]
                        [ View.EncryptionView.enigmaPreview model.enigma model.substitutionLog ]
                    , Html.div
                        [ View.StyleElements.mediumMargin
                        , View.StyleElements.flexGrow
                        ]
                        [ View.EncryptionView.lampBoardPreview model.substitutionLog
                        , View.EncryptionView.textInputBoxView model.messageHolder model.enigma.operationMode EncryptionMsg
                        ]
                    ]
                ]
            ]
        ]



-- ---------------------------------------------------------------------------------------------------------------------
-- Browser functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Return the initial model of the mainView
-}
initialModel : ( Model, Cmd Msg )
initialModel =
    ( Model.defaultModel
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
            View.ConfigurationView.update configurationMessage model ConfigurationMsg

        MessageHolderMsg serverMessageHolderMessage ->
            View.MessageHolderView.update serverMessageHolderMessage model MessageHolderMsg

        EncryptionMsg encryptionMsg ->
            View.EncryptionView.update encryptionMsg model


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
