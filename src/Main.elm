module Main exposing (main)

import Array
import Browser
import Dict
import Enigma.EnigmaMachine exposing (Enigma)
import Enigma.Rotor exposing (Rotor)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Utils.AlphabetHelper


type Msg
    = SubstituteChar Char
    | SetRotor Int (Maybe Rotor)
    | SetRotorPosition Int Int
    | SetRingPosition Int Int
    | ToggleMode


{-| Hold the string values for the raw and processed input and the out value
-}
type alias MessageHolder =
    { rawInput : String, prcessedInput : String, processedOutput : String }


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
            ]
        ]


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
            { rawInput = "Hello world", prcessedInput = "", processedOutput = "" }
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
