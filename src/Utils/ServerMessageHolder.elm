module Utils.ServerMessageHolder exposing (ServerMessageHolder, defaultServerMessageHolder)

import Utils.MessageHolder


type alias ServerMessageHolder =
    List Utils.MessageHolder.MessageHolder



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


defaultServerMessageHolder : ServerMessageHolder
defaultServerMessageHolder =
    List.repeat 100 Utils.MessageHolder.defaultMessageHolder



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------
