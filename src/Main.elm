module Main exposing (main)

import Browser
import Dict
import Enigma.EnigmaMachine exposing (Enigma)
import Enigma.Rotor exposing (Rotor)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Regex
import String
import Utils.AlphabetHelper
import Utils.MessageHolder exposing (MessageHolder)


type Msg
    = SubstituteChar Char
    | UpdateRawInput String
    | SetRotor Int (Maybe Rotor)
    | SetRotorPosition Int Int
    | SetRingPosition Int Int
    | ToggleMode


type Mode
    = Configuration
    | Encryption


type alias Model =
    { enigma : Enigma
    , messageHolder : MessageHolder
    , mode : Mode
    }



-- ---------------------------------------------------------------------------------------------------------------------
-- View
-- ---------------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.div
            []
            [ Html.h2 [ Html.Attributes.align "center" ] [ Html.text "Configuration" ]
            , configurationView model
            , toggleModeButton model
            ]
        , Html.div
            []
            [ Html.h2 [ Html.Attributes.align "center" ] [ Html.text "Preview" ]
            , enigmaPreview model
            , encryptionView model
            ]
        ]



-- ---------------------------------------------------------------------------------------------------------------------
-- Configuration View
-- ---------------------------------------------------------------------------------------------------------------------


configurationView : Model -> Html Msg
configurationView model =
    Html.div
        []
        [ Html.div
            []
            [ Html.h3 [] [ Html.text "Select rotor type" ]
            , selectRotorView model
            ]
        , Html.div
            []
            [ Html.h3 [] [ Html.text "Select rotor position" ]
            , selectRotorPositionView model
            ]
        , Html.div
            []
            [ Html.h3 [] [ Html.text "Select ring position" ]
            , selectRingPosition model
            ]
        ]


selectRotorView : Model -> Html Msg
selectRotorView model =
    Html.table
        []
        [ tableRowWithRotorNumbers model.enigma.rotors
        , Html.tr [] (List.indexedMap (displayRotorSelectionInTable model) model.enigma.rotors)
        ]


displayRotorSelectionInTable : Model -> Int -> Rotor -> Html Msg
displayRotorSelectionInTable model index rotor =
    Html.td
        []
        [ Html.select
            [ Html.Events.on "change" (Json.Decode.map (\val -> SetRotor index (Dict.get val Enigma.Rotor.getAllRotors)) Html.Events.targetValue)
            , enableAttributeWhenInConfiguration model
            ]
            (List.map
                (\currentRotor ->
                    Html.option
                        [ Html.Attributes.value currentRotor.name
                        , Html.Attributes.selected (currentRotor.name == rotor.name)
                        ]
                        [ Html.text currentRotor.name
                        ]
                )
                (Dict.values Enigma.Rotor.getAllRotors)
            )
        ]


selectRotorPositionView : Model -> Html Msg
selectRotorPositionView model =
    Html.table
        []
        [ tableRowWithRotorNumbers model.enigma.rotors
        , Html.tr [] (List.indexedMap (displayRotorPositionSelectionInTable model) model.enigma.rotors)
        ]


displayRotorPositionSelectionInTable : Model -> Int -> Rotor -> Html Msg
displayRotorPositionSelectionInTable model index rotor =
    Html.td
        []
        [ Html.select
            [ Html.Events.on "change" (Json.Decode.map (\val -> SetRotorPosition index (Maybe.withDefault 0 (String.toInt val))) Html.Events.targetValue)
            , enableAttributeWhenInConfiguration model
            ]
            (List.map
                (\position ->
                    Html.option
                        [ Html.Attributes.value (String.fromInt position)
                        , Html.Attributes.selected (rotor.startPosition == position)
                        ]
                        [ Html.text (String.fromChar (Maybe.withDefault '-' (Utils.AlphabetHelper.characterIndexToCharacter (Just position))))
                        ]
                )
                (List.range 0 25)
            )
        ]


selectRingPosition : Model -> Html Msg
selectRingPosition model =
    Html.table
        []
        [ tableRowWithRotorNumbers model.enigma.rotors
        , Html.tr [] (List.indexedMap (displayRingPositionSelectionInTable model) model.enigma.rotors)
        ]


displayRingPositionSelectionInTable : Model -> Int -> Rotor -> Html Msg
displayRingPositionSelectionInTable model index rotor =
    Html.td
        []
        [ Html.select
            [ Html.Events.on "change" (Json.Decode.map (\val -> SetRingPosition index (Maybe.withDefault 0 (String.toInt val))) Html.Events.targetValue)
            , enableAttributeWhenInConfiguration model
            ]
            (List.map
                (\position ->
                    Html.option
                        [ Html.Attributes.selected (rotor.ringPosition == position)
                        , Html.Attributes.value (String.fromInt position)
                        ]
                        [ Html.text (String.fromInt position) ]
                )
                (List.range 0 25)
            )
        ]


toggleModeButton : Model -> Html Msg
toggleModeButton model =
    Html.div []
        [ Html.button
            [ Html.Events.onClick ToggleMode ]
            [ case model.mode of
                Encryption ->
                    Html.text "Switch to Configuration Mode"

                Configuration ->
                    Html.text "Switch to Encryption Mode"
            ]
        ]


{-| get a table row with the rotors
-}
tableRowWithRotorNumbers : List Rotor -> Html Msg
tableRowWithRotorNumbers rotors =
    Html.tr [] (List.indexedMap (\index _ -> Html.td [] [ Html.text ("Rotor " ++ String.fromInt (index + 1)) ]) rotors)


{-| enable the element when the model is in configuration mode and disable the element when the model is in encryption mode
-}
enableAttributeWhenInConfiguration : Model -> Html.Attribute Msg
enableAttributeWhenInConfiguration model =
    case model.mode of
        Encryption ->
            Html.Attributes.disabled True

        Configuration ->
            Html.Attributes.disabled False


enigmaPreview : Model -> Html Msg
enigmaPreview model =
    case model.mode of
        Configuration ->
            Html.text "ConfigurationMode"

        Encryption ->
            Html.text "Encryption Mode"



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
        , Html.div
            []
            [ Html.h3 [] [ Html.text "Text Input" ]
            , textInputView model
            ]
        ]


encryptionResultView : Model -> Html Msg
encryptionResultView model =
    Html.table
        []
        [ Html.tr
            []
            [ Html.td [] [ Html.text "Processed Input: " ]
            , Html.td [] [ Html.text (Regex.replace (Maybe.withDefault Regex.never (Regex.fromString ".{5}")) (\match -> match.match ++ " ") model.messageHolder.prcessedInput) ]
            ]
        , Html.tr
            []
            [ Html.td [] [ Html.text "Processed Output: " ]
            , Html.td [] [ Html.text (Regex.replace (Maybe.withDefault Regex.never (Regex.fromString ".{5}")) (\match -> match.match ++ " ") model.messageHolder.processedOutput) ]
            ]
        ]


textInputView : Model -> Html Msg
textInputView model =
    Html.textarea
        [ Html.Attributes.placeholder "Copy your text here"
        , Html.Attributes.value model.messageHolder.rawInput
        , Html.Events.onInput UpdateRawInput
        ]
        []



-- ---------------------------------------------------------------------------------------------------------------------
-- Browser functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Return the initial model of the mainView
-}
initialModel : Model
initialModel =
    let
        enigma =
            Enigma.EnigmaMachine.debugEnigma

        messageHolder =
            { rawInput = "Hello world", prcessedInput = "IAMGTANOOB", processedOutput = "ASFQWLKJQLKJQRW" }
    in
    { enigma = enigma, messageHolder = messageHolder, mode = Configuration }


{-| Return the subscriptions for the given model
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


{-| Update the given model with the given msg
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRotor index maybeRotor ->
            case maybeRotor of
                Just rotor ->
                    ( Debug.log "SetRotorResult: " { model | enigma = Enigma.EnigmaMachine.replaceRotor model.enigma index rotor }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetRotorPosition rotorIndex newStartPosition ->
            ( { model | enigma = Debug.log "SetRotorPosition" (Enigma.EnigmaMachine.setStartPositionOfRotor model.enigma rotorIndex newStartPosition) }, Cmd.none )

        SetRingPosition rotorIndex newRingPosition ->
            ( { model | enigma = Debug.log "SetRingPosition" (Enigma.EnigmaMachine.setRingPositionOfRotor model.enigma rotorIndex newRingPosition) }, Cmd.none )

        ToggleMode ->
            let
                newMode =
                    case model.mode of
                        Encryption ->
                            Configuration

                        Configuration ->
                            Encryption
            in
            ( { model | mode = newMode }, Cmd.none )

        UpdateRawInput input ->
            ( { model | messageHolder = Utils.MessageHolder.updateRawInput model.messageHolder input }, Cmd.none )

        _ ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
