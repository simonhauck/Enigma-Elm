module View.ConfigurationView exposing (ConfigurationMsg(..), displayEnigmaConfiguration, update)

-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------

import Dict
import Enigma.EnigmaMachine as EnigmaMachine
import Enigma.Plugboard
import Enigma.Reflector
import Enigma.Rotor exposing (Rotor)
import Flip
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Utils.AlphabetHelper
import Utils.MessageHolder
import View.PlugBoardSvg
import View.StyleElements


type ConfigurationMsg
    = SetRotor Int Rotor
    | SetRotorPosition Int Int
    | SetRotorRingPosition Int Int
    | SetReflector Enigma.Reflector.Reflector
    | ResetPlugboard
    | PressCharOnPlugboard Enigma.Plugboard.CharPosition Int
    | ToggleForeignCharOption
    | ToggleOperationMode
    | StartRandomKeyGeneration
    | HandleRandomKeyGeneration EnigmaMachine.RandomizationType


type alias ConvertToMainMsgFunction msg =
    ConfigurationMsg -> msg


displayEnigmaConfiguration : EnigmaMachine.Enigma -> Utils.MessageHolder.MessageHolder -> ConvertToMainMsgFunction msg -> Html msg
displayEnigmaConfiguration enigma messageHolder convertToMainMsgFunction =
    Html.div
        []
        [ Html.h2 [ Html.Attributes.align "center" ] [ Html.text "Configuration" ]
        , configurationView enigma messageHolder convertToMainMsgFunction
        , toggleModeButton enigma convertToMainMsgFunction
        ]


update : ConfigurationMsg -> EnigmaMachine.Enigma -> Utils.MessageHolder.MessageHolder -> ConvertToMainMsgFunction msg -> ( EnigmaMachine.Enigma, Utils.MessageHolder.MessageHolder, Cmd msg )
update msg enigma messageHolder convertToMainMsgFunction =
    let
        buildTuple3 =
            \( a, b ) c -> ( a, b, c )
    in
    case msg of
        SetRotor index rotor ->
            ( EnigmaMachine.setRotor enigma index rotor, messageHolder, Cmd.none )

        SetRotorPosition index position ->
            ( EnigmaMachine.setStartPositionOfRotor enigma index position, messageHolder, Cmd.none )

        SetRotorRingPosition index position ->
            ( EnigmaMachine.setRingPositionOfRotor enigma index position, messageHolder, Cmd.none )

        SetReflector reflector ->
            ( EnigmaMachine.setReflector enigma reflector, messageHolder, Cmd.none )

        ResetPlugboard ->
            ( EnigmaMachine.resetPlugBoard enigma, messageHolder, Cmd.none )

        PressCharOnPlugboard charPosition charIndex ->
            ( EnigmaMachine.pressCharOnPlugBoard enigma charPosition charIndex, messageHolder, Cmd.none )

        ToggleForeignCharOption ->
            ( enigma, Utils.MessageHolder.toggleForeignCharOption messageHolder, Cmd.none )

        ToggleOperationMode ->
            toggleOperationMode enigma messageHolder |> Flip.flip buildTuple3 Cmd.none

        StartRandomKeyGeneration ->
            ( enigma, messageHolder, randomizeEnigma enigma convertToMainMsgFunction )

        HandleRandomKeyGeneration randomizationType ->
            ( EnigmaMachine.handleRandomizeResult enigma randomizationType, messageHolder, Cmd.none )



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


configurationView : EnigmaMachine.Enigma -> Utils.MessageHolder.MessageHolder -> ConvertToMainMsgFunction msg -> Html msg
configurationView enigma messageHolder convertToMainMsgFunction =
    Html.div
        []
        [ Html.div
            []
            [ Html.h3 [] [ Html.text "Select rotor type" ]
            , selectRotorView enigma convertToMainMsgFunction
            ]
        , Html.div
            []
            [ Html.h3 [] [ Html.text "Select rotor position" ]
            , selectRotorPositionView enigma convertToMainMsgFunction
            ]
        , Html.div
            []
            [ Html.h3 [] [ Html.text "Select ring position" ]
            , selectRingPositionView enigma convertToMainMsgFunction
            ]
        , Html.div
            []
            [ Html.h3 [] [ Html.text "Select reflector" ]
            , selectReflectorView enigma convertToMainMsgFunction
            ]
        , Html.div
            []
            [ Html.h3 [] [ Html.text "Configure plugboard" ]
            , configurePlugBoardView enigma convertToMainMsgFunction
            ]
        , Html.div
            []
            [ Html.h3 [] [ Html.text "Other configuration - Change later :D" ]
            , otherConfigurationView enigma messageHolder convertToMainMsgFunction
            ]
        ]


toggleOperationMode : EnigmaMachine.Enigma -> Utils.MessageHolder.MessageHolder -> ( EnigmaMachine.Enigma, Utils.MessageHolder.MessageHolder )
toggleOperationMode enigma messageHolder =
    case enigma.operationMode of
        EnigmaMachine.Encryption ->
            ( { enigma | operationMode = EnigmaMachine.Configuration } |> EnigmaMachine.setCurrentPositionToStartPosition
            , messageHolder
            )

        EnigmaMachine.Configuration ->
            ( { enigma | operationMode = EnigmaMachine.Encryption }, messageHolder )


{-| generate a command to completely randomize the enigma
-}
randomizeEnigma : EnigmaMachine.Enigma -> ConvertToMainMsgFunction msg -> Cmd msg
randomizeEnigma enigma convertToMainMsgFunction =
    Cmd.batch
        [ Enigma.Plugboard.randomPlugboardCmd
            (\shuffledList -> EnigmaMachine.RandomizePlugboard shuffledList |> HandleRandomKeyGeneration |> convertToMainMsgFunction)
        , EnigmaMachine.randomizeRotorsCmd
            (\randomRotorPair -> EnigmaMachine.RandomizeRotor randomRotorPair |> HandleRandomKeyGeneration |> convertToMainMsgFunction)
            enigma
        , EnigmaMachine.randomizeReflectorCmd
            (\randomReflector -> EnigmaMachine.RandomizeReflector randomReflector |> HandleRandomKeyGeneration |> convertToMainMsgFunction)
        , EnigmaMachine.getRandomCharPositionsCmd
            (\randomPosition -> EnigmaMachine.RandomizeRotorStartPosition randomPosition |> HandleRandomKeyGeneration |> convertToMainMsgFunction)
            enigma
        , EnigmaMachine.getRandomCharPositionsCmd
            (\randomPosition -> EnigmaMachine.RandomizeRotorRingPosition randomPosition |> HandleRandomKeyGeneration |> convertToMainMsgFunction)
            enigma
        ]



-- ---------------------------------------------------------------------------------------------------------------------
-- Select RotorType functions
-- ---------------------------------------------------------------------------------------------------------------------


selectRotorView : EnigmaMachine.Enigma -> ConvertToMainMsgFunction msg -> Html msg
selectRotorView enigma convertToMainMsgFunction =
    --    TODO Remove first Rotor?
    Html.table
        []
        [ tableRowWithRotorNumbers enigma.rotors
        , Html.tr [] (Html.td [] [ Html.text "Rotor:" ] :: List.indexedMap (displayRotorSelectionInTable enigma convertToMainMsgFunction) enigma.rotors)
        ]



--TODO change pipeline direction
--convertToMainMsgFunction <| SetRotor index <| Flip.flip Dict.get Enigma.Rotor.getAllRotors <| val


displayRotorSelectionInTable : EnigmaMachine.Enigma -> ConvertToMainMsgFunction msg -> Int -> Rotor -> Html msg
displayRotorSelectionInTable enigma convertToMainMsgFunction index rotor =
    Html.td
        View.StyleElements.selectWrapperStyleElements
        [ Html.select
            (Html.Events.on "change" (Json.Decode.map (\val -> Dict.get val Enigma.Rotor.getAllRotors |> Maybe.withDefault Enigma.Rotor.rotor1 |> SetRotor index |> convertToMainMsgFunction) Html.Events.targetValue)
                :: enableAttributeWhenInConfiguration enigma
                :: View.StyleElements.selectStyleElements
            )
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



-- ---------------------------------------------------------------------------------------------------------------------
-- Select RotorPosition functions
-- ---------------------------------------------------------------------------------------------------------------------


selectRotorPositionView : EnigmaMachine.Enigma -> ConvertToMainMsgFunction msg -> Html msg
selectRotorPositionView enigma convertToMainMsgFunction =
    Html.table
        []
        [ tableRowWithRotorNumbers enigma.rotors
        , Html.tr [] (Html.td [] [ Html.text "Start Position:" ] :: List.indexedMap (displayRotorPositionSelectionInTable enigma convertToMainMsgFunction) enigma.rotors)
        , Html.tr []
            (Html.td [] [ Html.text "Current Position:" ]
                :: List.map
                    (\rotor ->
                        Html.td []
                            [ Html.text (getNumberAndCharText rotor.currentPosition)
                            ]
                    )
                    enigma.rotors
            )
        ]



--            [ Html.Events.on "change" (Json.Decode.map (\val -> SetRotorPosition index (Maybe.withDefault 0 (String.toInt val))) Html.Events.targetValue)
--TODO change pipeleine direction and remove comment line
--val -> Json.Decode.map (convertToMainMsgFunction index <| Maybe.withDefault 0 <| String.toInt <| val) Html.Events.targetValue


displayRotorPositionSelectionInTable : EnigmaMachine.Enigma -> ConvertToMainMsgFunction msg -> Int -> Rotor -> Html msg
displayRotorPositionSelectionInTable enigma convertToMainMsgFunction index rotor =
    Html.td
        []
        [ Html.select
            [ Html.Events.on "change"
                (Flip.flip Json.Decode.map
                    Html.Events.targetValue
                    (\val ->
                        String.toInt val
                            |> Maybe.withDefault 0
                            |> SetRotorPosition index
                            |> convertToMainMsgFunction
                    )
                )
            , enableAttributeWhenInConfiguration enigma
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



-- ---------------------------------------------------------------------------------------------------------------------
-- Select RotorRingPosition functions
-- ---------------------------------------------------------------------------------------------------------------------


selectRingPositionView : EnigmaMachine.Enigma -> ConvertToMainMsgFunction msg -> Html msg
selectRingPositionView enigma convertToMainMsgFunction =
    Html.table
        []
        [ tableRowWithRotorNumbers enigma.rotors
        , Html.tr [] (Html.td [] [ Html.text "Ring Position:" ] :: List.indexedMap (displayRingPositionSelectionInTable enigma convertToMainMsgFunction) enigma.rotors)
        ]



--TODO Check and remove
--SetRingPosition index (Maybe.withDefault 0 (String.toInt val))
--Json.Decode.map (\val -> convertToMainMsgFunction index <| Maybe.withDefault 0 <| String.toInt <| val) Html.Events.targetValue


displayRingPositionSelectionInTable : EnigmaMachine.Enigma -> ConvertToMainMsgFunction msg -> Int -> Rotor -> Html msg
displayRingPositionSelectionInTable model convertToMainMsgFunction index rotor =
    Html.td
        []
        [ Html.select
            [ Html.Events.on "change"
                (Flip.flip Json.Decode.map
                    Html.Events.targetValue
                    (\val ->
                        String.toInt val
                            |> Maybe.withDefault 0
                            |> SetRotorRingPosition index
                            |> convertToMainMsgFunction
                    )
                )
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



-- ---------------------------------------------------------------------------------------------------------------------
-- Select Reflector functions
-- ---------------------------------------------------------------------------------------------------------------------
--Html.Events.onInput
--                            (\reflectorName ->
--                                SetReflector
--                                    (Maybe.withDefault Enigma.Reflector.reflectorA (Dict.get reflectorName Enigma.Reflector.getAllReflectors))
--                            )
--TODO CHange direction
--                                convertToMainMsgFunction <|
--                                    Maybe.withDefault Enigma.Reflector.reflectorB <|
--                                        Flip.flip Dict.get Enigma.Reflector.getAllReflectors <|
--                                            reflectorName


selectReflectorView : EnigmaMachine.Enigma -> ConvertToMainMsgFunction msg -> Html msg
selectReflectorView enigma convertToMainMsgFunction =
    Html.div
        []
        (List.map
            (\reflector ->
                Html.label
                    []
                    [ Html.input
                        [ Html.Attributes.type_ "radio"
                        , Html.Attributes.value reflector.name
                        , Html.Attributes.checked (reflector.name == enigma.reflector.name)
                        , Html.Events.onInput
                            (\reflectorName ->
                                Dict.get reflectorName Enigma.Reflector.getAllReflectors
                                    |> Maybe.withDefault Enigma.Reflector.reflectorB
                                    |> SetReflector
                                    |> convertToMainMsgFunction
                            )
                        , enableAttributeWhenInConfiguration enigma
                        ]
                        []
                    , Html.text reflector.name
                    ]
            )
            (Dict.values Enigma.Reflector.getAllReflectors)
        )



-- ---------------------------------------------------------------------------------------------------------------------
-- Configure Plugboard functions
-- ---------------------------------------------------------------------------------------------------------------------


configurePlugBoardView : EnigmaMachine.Enigma -> ConvertToMainMsgFunction msg -> Html msg
configurePlugBoardView enigma convertToMainMsgFunction =
    let
        sizePerCharacter =
            30
    in
    Html.div
        []
        [ Html.div
            [ Html.Attributes.width (sizePerCharacter * 27)
            ]
            [ Html.div
                []
                (plugBoardCharacterButtons enigma convertToMainMsgFunction Enigma.Plugboard.Input sizePerCharacter)
            , Html.div
                []
                [ View.PlugBoardSvg.plugBoardCanvas enigma.plugBoard sizePerCharacter ]
            , Html.div
                []
                (plugBoardCharacterButtons enigma convertToMainMsgFunction Enigma.Plugboard.Output sizePerCharacter)
            ]
        , Html.button
            [ enableAttributeWhenInConfiguration enigma
            , Html.Events.onClick <| convertToMainMsgFunction <| ResetPlugboard
            ]
            [ Html.text "Reset Plugboard" ]
        ]


plugBoardCharacterButtons : EnigmaMachine.Enigma -> ConvertToMainMsgFunction msg -> Enigma.Plugboard.CharPosition -> Int -> List (Html msg)
plugBoardCharacterButtons model convertToMainMsgFunction charPosition sizePerCharacter =
    List.map
        (\index ->
            Html.button
                [ enableAttributeWhenInConfiguration model
                , Html.Events.onClick <| convertToMainMsgFunction <| PressCharOnPlugboard charPosition index
                , Html.Attributes.style "height" "25px"
                , Html.Attributes.style "width" <| String.fromInt sizePerCharacter ++ "px"
                ]
                [ index |> Just |> Utils.AlphabetHelper.characterIndexToCharacter |> Maybe.withDefault '-' |> String.fromChar |> Html.text ]
        )
        (List.range 0 25)



-- ---------------------------------------------------------------------------------------------------------------------
-- Other Configuration functions
-- ---------------------------------------------------------------------------------------------------------------------


otherConfigurationView : EnigmaMachine.Enigma -> Utils.MessageHolder.MessageHolder -> ConvertToMainMsgFunction msg -> Html msg
otherConfigurationView enigma messageHolder convertToMainMsgFunction =
    Html.div
        []
        [ Html.label []
            [ Html.input
                [ Html.Attributes.type_ "checkBox"
                , enableAttributeWhenInConfiguration enigma
                , Html.Attributes.checked (messageHolder.foreignCharOption == Utils.MessageHolder.Include)
                , Html.Events.onClick (convertToMainMsgFunction ToggleForeignCharOption)
                ]
                []
            , Html.text "Include foreign chars"
            ]
        , Html.button
            [ enableAttributeWhenInConfiguration enigma
            , Html.Events.onClick (convertToMainMsgFunction StartRandomKeyGeneration)
            ]
            [ Html.text "Generate random key" ]
        ]


toggleModeButton : EnigmaMachine.Enigma -> ConvertToMainMsgFunction msg -> Html msg
toggleModeButton enigma convertToMainMsgFunction =
    Html.div []
        [ Html.button
            [ Html.Events.onClick (convertToMainMsgFunction ToggleOperationMode) ]
            [ case enigma.operationMode of
                EnigmaMachine.Encryption ->
                    Html.text "Switch to Configuration Mode"

                EnigmaMachine.Configuration ->
                    Html.text "Switch to Encryption Mode"
            ]
        ]



-- ---------------------------------------------------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| get a table row with the rotors
-}
tableRowWithRotorNumbers : List Rotor -> Html msg
tableRowWithRotorNumbers rotors =
    Html.tr
        []
        (Html.td [] [] :: List.indexedMap (\index _ -> Html.td [] [ index + 1 |> String.fromInt |> String.append "Rotor " |> Html.text ]) rotors)


{-| Get an Html disabled attribute. The value is true, when the enigma is in the encryption mode
-}
enableAttributeWhenInConfiguration : EnigmaMachine.Enigma -> Html.Attribute msg
enableAttributeWhenInConfiguration enigma =
    case enigma.operationMode of
        EnigmaMachine.Encryption ->
            Html.Attributes.disabled True

        EnigmaMachine.Configuration ->
            Html.Attributes.disabled False


{-| Return a text that contains the number and the corresponding char
-}
getNumberAndCharText : Int -> String
getNumberAndCharText number =
    number
        |> Just
        |> Utils.AlphabetHelper.characterIndexToCharacter
        |> Maybe.withDefault '-'
        |> String.fromChar
        |> String.append " - "
        |> String.append (String.fromInt number)
