module Models.Enigma.OperationMode exposing (OperationMode(..), toggleOperationMode)


type OperationMode
    = Configuration
    | Encryption


{-| Toggle the OperationMode between Configuration and Encryption
-}
toggleOperationMode : OperationMode -> OperationMode
toggleOperationMode operationMode =
    case operationMode of
        Configuration ->
            Encryption

        Encryption ->
            Configuration
