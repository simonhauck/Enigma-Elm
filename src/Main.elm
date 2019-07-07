module Main exposing (main)

import Browser
import Enigma.EnigmaMachine exposing (Enigma)
import Html exposing (Html)


type Msg
    = SubstituteChar Char


type alias Model =
    { enigma : Enigma, rawInput : String, processedInput : String, processedOutput : String }



-- ---------------------------------------------------------------------------------------------------------------------
-- View
-- ---------------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    Html.div [] [ Html.text model.rawInput ]



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

        rawInput =
            "Hello world"

        processedINput =
            ""

        processedOutput =
            ""
    in
    { enigma = enigma, rawInput = rawInput, processedInput = processedINput, processedOutput = processedOutput }


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
        SubstituteChar inputChar ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
