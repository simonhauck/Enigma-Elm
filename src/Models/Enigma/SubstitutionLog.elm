module Models.Enigma.SubstitutionLog exposing
    ( PostProcessing(..)
    , SubstitutionLog
    , UpdateLogFunction
    , addPlugboardInputSubstitution
    , addPlugboardOutputSubstitution
    , addReflectorSubstitution
    , addRotorFromReflectorSubstitution
    , addRotorToReflectorSubstitution
    , emptySubstitutionLog
    , getResultChar
    , postProcessLog
    )


type alias SubstitutionLog =
    { postProcessing : PostProcessing
    , plugboardInputSubstitution : ( Int, Int )
    , plugboardOutputSubstitution : ( Int, Int )
    , reflectorSubstitution : ( Int, Int )
    , rotorToReflectorSubstitution : List ( Int, Int )
    , rotorFromReflectorSubstitution : List ( Int, Int )
    }


{-| indicates if the Log is cleaned and modified to display the actual data
-}
type PostProcessing
    = InProgress
    | Finished


type alias UpdateLogFunction =
    ( Int, Int ) -> Maybe SubstitutionLog -> Maybe SubstitutionLog



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Get the result char of the substitution in the log
-}
getResultChar : SubstitutionLog -> Int
getResultChar log =
    Tuple.second log.plugboardOutputSubstitution


{-| Return an empty SubstitutionLog with default values
-}
emptySubstitutionLog : SubstitutionLog
emptySubstitutionLog =
    { postProcessing = InProgress
    , plugboardInputSubstitution = ( 0, 0 )
    , plugboardOutputSubstitution = ( 0, 0 )
    , reflectorSubstitution = ( 0, 0 )
    , rotorToReflectorSubstitution = []
    , rotorFromReflectorSubstitution = []
    }


{-| Process the SubstitutionLog. In the rotorList must be updated to display the values correctly.
The state of the SubstitutionLog will be changed to finished. If the SubstitutionLog is already in the state
Finished, the SubstitutionLog will not be modified
If the given Log is Nothing, Nothing will be returned.
-}
postProcessLog : Maybe SubstitutionLog -> Maybe SubstitutionLog
postProcessLog maybeSubstitutionLog =
    case maybeSubstitutionLog of
        Nothing ->
            Nothing

        Just log ->
            case log.postProcessing of
                Finished ->
                    Just log

                InProgress ->
                    let
                        rotorToReflector =
                            log.rotorToReflectorSubstitution
                                |> List.tail
                                |> Maybe.withDefault []

                        rotorFromReflector =
                            log.rotorFromReflectorSubstitution
                                |> List.tail
                                |> Maybe.withDefault []
                                |> List.reverse
                    in
                    Just
                        { log
                            | postProcessing = Finished
                            , rotorToReflectorSubstitution = rotorToReflector
                            , rotorFromReflectorSubstitution = rotorFromReflector
                        }


{-| Function to add the plugboard value for the substitution in direction to the reflector
-}
addPlugboardInputSubstitution : UpdateLogFunction
addPlugboardInputSubstitution substitutionPair =
    unwrapSubstitutionLog (\log -> { log | plugboardInputSubstitution = substitutionPair })


{-| Function to add the plugboard value for the substitution in direction from the reflector
-}
addPlugboardOutputSubstitution : UpdateLogFunction
addPlugboardOutputSubstitution substitutionPair =
    unwrapSubstitutionLog (\log -> { log | plugboardOutputSubstitution = substitutionPair })


{-| Function to add the reflector value for the substitution to the log
-}
addReflectorSubstitution : UpdateLogFunction
addReflectorSubstitution substitutionPair =
    unwrapSubstitutionLog (\log -> { log | reflectorSubstitution = substitutionPair })


{-| Function to add a pair of the rotor substitution in direction to the reflector to the log
-}
addRotorToReflectorSubstitution : UpdateLogFunction
addRotorToReflectorSubstitution substitutionPair =
    unwrapSubstitutionLog (\log -> { log | rotorToReflectorSubstitution = substitutionPair :: log.rotorToReflectorSubstitution })


{-| Function to add a pair of the rotor substitution in direction from the reflector to the log
-}
addRotorFromReflectorSubstitution : UpdateLogFunction
addRotorFromReflectorSubstitution substitutionPair =
    unwrapSubstitutionLog (\log -> { log | rotorFromReflectorSubstitution = substitutionPair :: log.rotorFromReflectorSubstitution })



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Get the maybe SubstitutionLog. If it is not nothing, apply the given function and return the log as maybe value.
If the value is nothing, Nothing will be returned
-}
unwrapSubstitutionLog : (SubstitutionLog -> SubstitutionLog) -> Maybe SubstitutionLog -> Maybe SubstitutionLog
unwrapSubstitutionLog f =
    let
        unwrapHelper =
            Maybe.map (f >> Just) >> Maybe.withDefault Nothing
    in
    unwrapHelper
