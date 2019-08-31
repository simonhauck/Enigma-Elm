module View.ConfigurationView exposing (ConfigurationMsg(..), displayEnigmaConfiguration, update)

import Dict
import Flip
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Models.Enigma.EnigmaMachine as EnigmaMachine
import Models.Enigma.OperationMode as OperationMode
import Models.Enigma.Plugboard as Plugboard
import Models.Enigma.Reflector as Reflector
import Models.Enigma.Rotor as Rotor
import Models.MessageHolder as MessageHolder
import Utils.AlphabetHelper
import View.HtmlComponents
import View.PlugBoardSvg
import View.StyleElements


type ConfigurationMsg
    = SetRotor Int Rotor.Rotor
    | SetRotorPosition Int Int
    | SetRotorRingPosition Int Int
    | SetReflector Reflector.Reflector
    | ResetPlugboard
    | PressCharOnPlugboard Plugboard.CharPosition Int
    | ToggleForeignCharOption
    | ToggleOperationMode
    | StartRandomKeyGeneration
    | HandleRandomKeyGeneration EnigmaMachine.RandomizationType


type alias ConvertConfigurationMsg msg =
    ConfigurationMsg -> msg



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


displayEnigmaConfiguration : EnigmaMachine.Enigma -> MessageHolder.MessageHolder -> ConvertConfigurationMsg msg -> Html msg
displayEnigmaConfiguration enigma messageHolder convertToMainMsgFunction =
    Html.div
        View.StyleElements.flexDirectionColumn
        [ Html.h2 View.StyleElements.h2StyleElements [ Html.text "Configuration" ]
        , configurationView enigma messageHolder convertToMainMsgFunction
        , toggleModeButton enigma convertToMainMsgFunction
        ]


update :
    ConfigurationMsg
    -> EnigmaMachine.Enigma
    -> MessageHolder.MessageHolder
    -> ConvertConfigurationMsg msg
    -> ( EnigmaMachine.Enigma, MessageHolder.MessageHolder, Cmd msg )
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
            ( enigma, MessageHolder.toggleForeignCharOption messageHolder, Cmd.none )

        ToggleOperationMode ->
            toggleOperationMode enigma messageHolder |> Flip.flip buildTuple3 Cmd.none

        StartRandomKeyGeneration ->
            ( enigma, messageHolder, randomizeEnigma enigma convertToMainMsgFunction )

        HandleRandomKeyGeneration randomizationType ->
            ( EnigmaMachine.handleRandomizeResult enigma randomizationType, messageHolder, Cmd.none )



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


configurationView : EnigmaMachine.Enigma -> MessageHolder.MessageHolder -> ConvertConfigurationMsg msg -> Html msg
configurationView enigma messageHolder convertToMainMsgFunction =
    Html.div
        []
        [ Html.div
            View.StyleElements.flexDisplay
            [ Html.div
                View.StyleElements.smallMargin
                [ Html.div
                    View.StyleElements.smallElementBox
                    [ Html.h3 View.StyleElements.h3StyleElements [ Html.text "Select rotor type" ]
                    , selectRotorView enigma convertToMainMsgFunction
                    ]
                , Html.div
                    View.StyleElements.smallElementBox
                    [ Html.h3 View.StyleElements.h3StyleElements [ Html.text "Select rotor position" ]
                    , selectRotorPositionView enigma convertToMainMsgFunction
                    ]
                , Html.div
                    View.StyleElements.smallElementBox
                    [ Html.h3 View.StyleElements.h3StyleElements [ Html.text "Select ring position" ]
                    , selectRingPositionView enigma convertToMainMsgFunction
                    ]
                , Html.div
                    View.StyleElements.smallElementBox
                    [ Html.h3 View.StyleElements.h3StyleElements [ Html.text "Select reflector" ]
                    , selectReflectorView enigma convertToMainMsgFunction
                    ]
                ]
            , Html.div
                View.StyleElements.smallMargin
                [ Html.div
                    View.StyleElements.smallElementBox
                    [ Html.h3 View.StyleElements.h3StyleElements [ Html.text "Configure plugboard" ]
                    , configurePlugBoardView enigma convertToMainMsgFunction
                    ]
                , Html.div
                    View.StyleElements.smallElementBox
                    [ Html.h3 View.StyleElements.h3StyleElements [ Html.text "Other configuration - Change later :D" ]
                    , otherConfigurationView enigma messageHolder convertToMainMsgFunction
                    ]
                ]
            ]
        ]


toggleOperationMode : EnigmaMachine.Enigma -> MessageHolder.MessageHolder -> ( EnigmaMachine.Enigma, MessageHolder.MessageHolder )
toggleOperationMode enigma messageHolder =
    let
        toggledEnigma =
            { enigma | operationMode = OperationMode.toggleOperationMode enigma.operationMode }
    in
    case toggledEnigma.operationMode of
        OperationMode.Encryption ->
            ( toggledEnigma |> EnigmaMachine.setStartPositionAsCurrentPosition
            , messageHolder
            )

        OperationMode.Configuration ->
            ( toggledEnigma, messageHolder |> MessageHolder.setProcessedInputAsRawInput )


{-| generate a command to completely randomize the enigma
-}
randomizeEnigma : EnigmaMachine.Enigma -> ConvertConfigurationMsg msg -> Cmd msg
randomizeEnigma enigma convertToMainMsgFunction =
    Cmd.batch
        [ Plugboard.randomPlugboardCmd
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


selectRotorView : EnigmaMachine.Enigma -> ConvertConfigurationMsg msg -> Html msg
selectRotorView enigma convertToMainMsgFunction =
    --    TODO Remove first Rotor?
    Html.table
        []
        [ tableRowWithRotorNumbers enigma.rotors
        , Html.tr [] (Html.td [] [ Html.text "Rotor:" ] :: List.indexedMap (displayRotorSelectionInTable enigma convertToMainMsgFunction) enigma.rotors)
        ]



--TODO change pipeline direction
--convertToMainMsgFunction <| SetRotor index <| Flip.flip Dict.get Enigma.Rotor.getAllRotors <| val


displayRotorSelectionInTable : EnigmaMachine.Enigma -> ConvertConfigurationMsg msg -> Int -> Rotor.Rotor -> Html msg
displayRotorSelectionInTable enigma convertToMainMsgFunction index rotor =
    Html.td
        View.StyleElements.selectWrapperStyleElements
        [ Html.select
            (Html.Events.on "change" (Json.Decode.map (\val -> Dict.get val Rotor.getAllRotors |> Maybe.withDefault Rotor.rotor1 |> SetRotor index |> convertToMainMsgFunction) Html.Events.targetValue)
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
                (Dict.values Rotor.getAllRotors)
            )
        ]



-- ---------------------------------------------------------------------------------------------------------------------
-- Select RotorPosition functions
-- ---------------------------------------------------------------------------------------------------------------------


selectRotorPositionView : EnigmaMachine.Enigma -> ConvertConfigurationMsg msg -> Html msg
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


displayRotorPositionSelectionInTable : EnigmaMachine.Enigma -> ConvertConfigurationMsg msg -> Int -> Rotor.Rotor -> Html msg
displayRotorPositionSelectionInTable enigma convertToMainMsgFunction index rotor =
    Html.td
        []
        [ Html.select
            (Html.Events.on "change"
                (Flip.flip Json.Decode.map
                    Html.Events.targetValue
                    (\val ->
                        String.toInt val
                            |> Maybe.withDefault 0
                            |> SetRotorPosition index
                            |> convertToMainMsgFunction
                    )
                )
                :: enableAttributeWhenInConfiguration enigma
                :: View.StyleElements.selectStyleElements
            )
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


selectRingPositionView : EnigmaMachine.Enigma -> ConvertConfigurationMsg msg -> Html msg
selectRingPositionView enigma convertToMainMsgFunction =
    Html.table
        []
        [ tableRowWithRotorNumbers enigma.rotors
        , Html.tr [] (Html.td [] [ Html.text "Ring Position:" ] :: List.indexedMap (displayRingPositionSelectionInTable enigma convertToMainMsgFunction) enigma.rotors)
        ]



--TODO Check and remove
--SetRingPosition index (Maybe.withDefault 0 (String.toInt val))
--Json.Decode.map (\val -> convertToMainMsgFunction index <| Maybe.withDefault 0 <| String.toInt <| val) Html.Events.targetValue


displayRingPositionSelectionInTable : EnigmaMachine.Enigma -> ConvertConfigurationMsg msg -> Int -> Rotor.Rotor -> Html msg
displayRingPositionSelectionInTable model convertToMainMsgFunction index rotor =
    Html.td
        []
        [ Html.select
            (Html.Events.on "change"
                (Flip.flip Json.Decode.map
                    Html.Events.targetValue
                    (\val ->
                        String.toInt val
                            |> Maybe.withDefault 0
                            |> SetRotorRingPosition index
                            |> convertToMainMsgFunction
                    )
                )
                :: enableAttributeWhenInConfiguration model
                :: View.StyleElements.selectStyleElements
            )
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
--TODO Custom Radio Buttons?


selectReflectorView : EnigmaMachine.Enigma -> ConvertConfigurationMsg msg -> Html msg
selectReflectorView enigma convertToMainMsgFunction =
    Html.div
        ([ Html.Attributes.style "display" "flex"
         , Html.Attributes.style "justify-content" "space-evenly"
         ]
            ++ View.StyleElements.fontFamily
            ++ View.StyleElements.fontColor
        )
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
                                Dict.get reflectorName Reflector.getAllReflectors
                                    |> Maybe.withDefault Reflector.reflectorB
                                    |> SetReflector
                                    |> convertToMainMsgFunction
                            )
                        , enableAttributeWhenInConfiguration enigma
                        ]
                        []
                    , Html.text reflector.name
                    ]
            )
            (Dict.values Reflector.getAllReflectors)
        )



-- ---------------------------------------------------------------------------------------------------------------------
-- Configure Plugboard functions
-- ---------------------------------------------------------------------------------------------------------------------


configurePlugBoardView : EnigmaMachine.Enigma -> ConvertConfigurationMsg msg -> Html msg
configurePlugBoardView enigma convertToMainMsgFunction =
    let
        sizePerCharacter =
            25

        marginButtons =
            2

        spaceBetween =
            marginButtons * 2 + sizePerCharacter
    in
    Html.div
        []
        [ Html.div
            [ Html.Attributes.width (spaceBetween * 27)
            ]
            [ Html.div
                []
                (plugBoardCharacterButtons enigma convertToMainMsgFunction Plugboard.Input sizePerCharacter marginButtons)
            , Html.div
                []
                [ View.PlugBoardSvg.plugBoardCanvas enigma.plugBoard spaceBetween ]
            , Html.div
                []
                (plugBoardCharacterButtons enigma convertToMainMsgFunction Plugboard.Output sizePerCharacter marginButtons)
            ]
        , Html.button
            (enableAttributeWhenInConfiguration enigma
                :: (Html.Events.onClick <| convertToMainMsgFunction <| ResetPlugboard)
                :: View.StyleElements.buttonStyleElements
            )
            [ Html.text "Reset Plugboard" ]
        ]


plugBoardCharacterButtons : EnigmaMachine.Enigma -> ConvertConfigurationMsg msg -> Plugboard.CharPosition -> Int -> Int -> List (Html msg)
plugBoardCharacterButtons model convertToMainMsgFunction charPosition sizePerCharacter marginButtons =
    List.map
        (\index ->
            Html.button
                (enableAttributeWhenInConfiguration model
                    :: (Html.Events.onClick <| convertToMainMsgFunction <| PressCharOnPlugboard charPosition index)
                    :: Html.Attributes.style "height" "25px"
                    :: (Html.Attributes.style "width" <| String.fromInt sizePerCharacter ++ "px")
                    :: (Html.Attributes.style "margin" <| String.fromInt marginButtons ++ "px")
                    :: View.StyleElements.plugboardButtonStyleElements
                )
                [ index |> Just |> Utils.AlphabetHelper.characterIndexToCharacter |> Maybe.withDefault '-' |> String.fromChar |> Html.text ]
        )
        (List.range 0 25)



-- ---------------------------------------------------------------------------------------------------------------------
-- Other Configuration functions
-- ---------------------------------------------------------------------------------------------------------------------


otherConfigurationView : EnigmaMachine.Enigma -> MessageHolder.MessageHolder -> ConvertConfigurationMsg msg -> Html msg
otherConfigurationView enigma messageHolder convertToMainMsgFunction =
    Html.div
        []
        [ Html.label []
            [ Html.input
                [ Html.Attributes.type_ "checkBox"
                , enableAttributeWhenInConfiguration enigma
                , Html.Attributes.checked (messageHolder.foreignCharOption == MessageHolder.Include)
                , Html.Events.onClick (convertToMainMsgFunction ToggleForeignCharOption)
                ]
                []
            , Html.text "Include foreign chars"
            ]
        , View.HtmlComponents.checkBox [] []
        , Html.button
            (enableAttributeWhenInConfiguration enigma
                :: Html.Events.onClick (convertToMainMsgFunction StartRandomKeyGeneration)
                :: View.StyleElements.buttonStyleElements
            )
            [ Html.text "Generate random key" ]
        ]


toggleModeButton : EnigmaMachine.Enigma -> ConvertConfigurationMsg msg -> Html msg
toggleModeButton enigma convertToMainMsgFunction =
    Html.div []
        [ Html.button
            (Html.Events.onClick (convertToMainMsgFunction ToggleOperationMode)
                :: View.StyleElements.buttonStyleElements
            )
            [ case enigma.operationMode of
                OperationMode.Encryption ->
                    Html.text "Switch to Configuration Mode"

                OperationMode.Configuration ->
                    Html.text "Switch to Encryption Mode"
            ]
        ]



-- ---------------------------------------------------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| get a table row with the rotors
-}
tableRowWithRotorNumbers : List Rotor.Rotor -> Html msg
tableRowWithRotorNumbers rotors =
    Html.tr
        []
        (Html.td [] [] :: List.indexedMap (\index _ -> Html.td [] [ index + 1 |> String.fromInt |> String.append "Rotor " |> Html.text ]) rotors)


{-| Get an Html disabled attribute. The value is true, when the enigma is in the encryption mode
-}
enableAttributeWhenInConfiguration : EnigmaMachine.Enigma -> Html.Attribute msg
enableAttributeWhenInConfiguration enigma =
    case enigma.operationMode of
        OperationMode.Encryption ->
            Html.Attributes.disabled True

        OperationMode.Configuration ->
            Html.Attributes.disabled False


{-| Return a text that contains the number + 1 and the corresponding char
-}
getNumberAndCharText : Int -> String
getNumberAndCharText number =
    Just number
        |> Utils.AlphabetHelper.characterIndexToCharacter
        |> Maybe.withDefault '-'
        |> String.fromChar
        |> String.append " - "
        |> String.append (String.fromInt (number + 1))
