module Models.Enigma.OperationMode exposing (OperationMode(..), toggleOperationMode)


type OperationMode
    = Configuration
    | Encryption


toggleOperationMode : OperationMode -> OperationMode
toggleOperationMode operationMode =
    case operationMode of
        Configuration ->
            Encryption

        Encryption ->
            Configuration
