module Main exposing (main)

import Browser
import Dict
import Enigma.EnigmaMachine exposing (Enigma)
import Enigma.Reflector exposing (Reflector)
import Enigma.Rotor exposing (Rotor)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import String
import Time
import Utils.AlphabetHelper
import Utils.MessageHolder exposing (ForeignChar, MessageHolder)


type Msg
    = EncryptCharTick
    | UpdateRawInput String
    | SetRotor Int (Maybe Rotor)
    | SetReflector Reflector
    | SetRotorPosition Int Int
    | SetRingPosition Int Int
    | ToggleForeignCharOption
    | ToggleOperationMode
    | ToggleEncryptionMode
    | SetEncryptionModeSpeed Int


type OperationMode
    = Configuration
    | Encryption


type EncryptionMode
    = Automatic
    | Manual


type alias TextInputConfig =
    { encryptionMode : EncryptionMode, encryptionSpeed : Int }


type alias Model =
    { enigma : Enigma
    , messageHolder : MessageHolder
    , operationMode : OperationMode
    , textInputConfig : TextInputConfig
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
            , selectRingPositionView model
            ]
        , Html.div
            []
            [ Html.h3 [] [ Html.text "Select reflector" ]
            , selectReflectorView model
            ]
        , Html.div
            []
            [ Html.h3 [] [ Html.text "Other configuration - Change later :D" ]
            , otherConfigurationView model
            ]
        ]


selectRotorView : Model -> Html Msg
selectRotorView model =
    Html.table
        []
        [ tableRowWithRotorNumbers model.enigma.rotors
        , Html.tr [] (Html.td [] [ Html.text "Rotor:" ] :: List.indexedMap (displayRotorSelectionInTable model) model.enigma.rotors)
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
        , Html.tr [] (Html.td [] [ Html.text "Start Position:" ] :: List.indexedMap (displayRotorPositionSelectionInTable model) model.enigma.rotors)
        , Html.tr []
            (Html.td [] [ Html.text "Current Position:" ]
                :: List.map
                    (\rotor ->
                        Html.td []
                            [ Html.text (getNumberAndCharText rotor.currentPosition)
                            ]
                    )
                    model.enigma.rotors
            )
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
                        [ Html.text (getNumberAndCharText position)
                        ]
                )
                (List.range 0 25)
            )
        ]


selectRingPositionView : Model -> Html Msg
selectRingPositionView model =
    Html.table
        []
        [ tableRowWithRotorNumbers model.enigma.rotors
        , Html.tr [] (Html.td [] [ Html.text "Ring Position:" ] :: List.indexedMap (displayRingPositionSelectionInTable model) model.enigma.rotors)
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
                        [ Html.text (getNumberAndCharText position) ]
                )
                (List.range 0 25)
            )
        ]


selectReflectorView : Model -> Html Msg
selectReflectorView model =
    Html.div
        []
        (List.map
            (\rotor ->
                Html.label
                    []
                    [ Html.input
                        [ Html.Attributes.type_ "radio"
                        , Html.Attributes.value rotor.name
                        , Html.Attributes.checked (rotor.name == model.enigma.reflector.name)
                        , enableAttributeWhenInConfiguration model
                        , Html.Events.onInput
                            (\reflectorName ->
                                SetReflector
                                    (Maybe.withDefault Enigma.Reflector.reflectorA (Dict.get reflectorName Enigma.Reflector.getAllReflectors))
                            )
                        ]
                        []
                    , Html.text rotor.name
                    ]
            )
            (Dict.values Enigma.Reflector.getAllReflectors)
        )


otherConfigurationView : Model -> Html Msg
otherConfigurationView model =
    Html.div
        []
        [ Html.label []
            [ Html.input
                [ Html.Attributes.type_ "checkBox"
                , enableAttributeWhenInConfiguration model
                , Html.Attributes.checked (model.messageHolder.foreignCharOption == Utils.MessageHolder.Include)
                , Html.Events.onClick ToggleForeignCharOption
                ]
                []
            , Html.text "Include foreign chars"
            ]
        ]


toggleModeButton : Model -> Html Msg
toggleModeButton model =
    Html.div []
        [ Html.button
            [ Html.Events.onClick ToggleOperationMode ]
            [ case model.operationMode of
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
    Html.tr [] (Html.td [] [] :: List.indexedMap (\index _ -> Html.td [] [ Html.text ("Rotor " ++ String.fromInt (index + 1)) ]) rotors)


{-| enable the element when the model is in configuration mode and disable the element when the model is in encryption mode
-}
enableAttributeWhenInConfiguration : Model -> Html.Attribute Msg
enableAttributeWhenInConfiguration model =
    case model.operationMode of
        Encryption ->
            Html.Attributes.disabled True

        Configuration ->
            Html.Attributes.disabled False


enigmaPreview : Model -> Html Msg
enigmaPreview model =
    case model.operationMode of
        Configuration ->
            Html.text "ConfigurationMode"

        Encryption ->
            Html.text "Encryption Mode"


{-| Return a text
-}
getNumberAndCharText : Int -> String
getNumberAndCharText number =
    String.fromInt number ++ " - " ++ String.fromChar (Maybe.withDefault '-' (Utils.AlphabetHelper.characterIndexToCharacter (Just number)))



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
        ]


encryptionResultView : Model -> Html Msg
encryptionResultView model =
    let
        ( formattedInput, formattedOutput ) =
            Utils.MessageHolder.getFormattedProcessedInputOutput model.messageHolder
    in
    Html.table
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


textInputField : Model -> Html Msg
textInputField model =
    Html.textarea
        [ Html.Attributes.placeholder "Copy your text here"
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
    case model.operationMode of
        Encryption ->
            Html.Attributes.disabled False

        Configuration ->
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

        ( newEnigma, maybeOutputChar ) =
            Enigma.EnigmaMachine.performRotationAndSubstitution model.enigma inputChar

        updatedMessageHolder =
            Utils.MessageHolder.addProcessedChar model.messageHolder inputChar maybeOutputChar
    in
    { model | enigma = newEnigma, messageHolder = updatedMessageHolder }



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
            { rawInput = "Hello world", processedInput = "", processedOutput = "", foreignCharOption = Utils.MessageHolder.Include }

        textInputConfig =
            { encryptionMode = Manual, encryptionSpeed = 250 }
    in
    { enigma = enigma, messageHolder = messageHolder, operationMode = Configuration, textInputConfig = textInputConfig }


{-| Return the subscriptions for the given model
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case ( model.operationMode, model.textInputConfig.encryptionMode ) of
        ( Encryption, Automatic ) ->
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
        SetRotor index maybeRotor ->
            case maybeRotor of
                Just rotor ->
                    ( Debug.log "SetRotorResult: " { model | enigma = Enigma.EnigmaMachine.replaceRotor model.enigma index rotor }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetRotorPosition rotorIndex newStartPosition ->
            ( { model | enigma = Debug.log "SetRotorPosition" (Enigma.EnigmaMachine.setStartPositionOfRotor model.enigma rotorIndex newStartPosition) }, Cmd.none )

        --        //TODO Remove Log statements at all
        SetRingPosition rotorIndex newRingPosition ->
            ( { model | enigma = Debug.log "SetRingPosition" (Enigma.EnigmaMachine.setRingPositionOfRotor model.enigma rotorIndex newRingPosition) }, Cmd.none )

        SetReflector newReflector ->
            ( { model | enigma = Debug.log "SetReflector" (Enigma.EnigmaMachine.replaceReflector model.enigma newReflector) }, Cmd.none )

        ToggleForeignCharOption ->
            ( { model | messageHolder = Utils.MessageHolder.toggleForeignCharOption model.messageHolder }, Cmd.none )

        ToggleOperationMode ->
            let
                ( updatedEnigma, newMode ) =
                    case model.operationMode of
                        Configuration ->
                            ( Enigma.EnigmaMachine.setCurrentPositionToStartPosition model.enigma
                            , Encryption
                            )

                        Encryption ->
                            ( model.enigma, Configuration )
            in
            ( { model | enigma = updatedEnigma, operationMode = newMode }, Cmd.none )

        UpdateRawInput input ->
            ( { model | messageHolder = Utils.MessageHolder.updateRawInput model.messageHolder input }, Cmd.none )

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


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
