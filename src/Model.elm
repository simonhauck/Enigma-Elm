module Model exposing (Model, defaultModel)

import Models.Enigma.EnigmaMachine as EnigmaMachine
import Models.Enigma.SubstitutionLog as Log
import Models.MessageHolder as MessageHolder
import Models.ServerMessageHolder as ServerMessageHolder


type alias Model =
    { enigma : EnigmaMachine.Enigma
    , substitutionLog : Maybe Log.SubstitutionLog
    , messageHolder : MessageHolder.MessageHolder
    , serverMessageHolder : ServerMessageHolder.ServerMessageHolder
    }



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Return the default model
-}
defaultModel : Model
defaultModel =
    { enigma = EnigmaMachine.defaultEnigma
    , substitutionLog = Nothing
    , messageHolder = MessageHolder.defaultMessageHolder
    , serverMessageHolder = ServerMessageHolder.defaultServerMessageHolder
    }



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------
