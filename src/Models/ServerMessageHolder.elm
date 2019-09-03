module Models.ServerMessageHolder exposing
    ( Filter
    , ServerMessageHolder
    , ServerState(..)
    , defaultServerMessageHolder
    , handleServerResponse
    , requestServerMessages
    , sendMessageToServer
    , setState
    )

import Http
import Json.Decode
import Json.Encode
import Models.MessageHolder as MessageHolder


type alias ServerMessageHolder =
    { filter : Filter, serverState : ServerState }


type ServerState
    = Loading
    | MessageHolderList (List MessageHolder.MessageHolder)
    | Error


type alias Filter =
    String



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Get the default ServerMessageHolder. Default is no filter and loading
-}
defaultServerMessageHolder : ServerMessageHolder
defaultServerMessageHolder =
    { filter = emptyFilter, serverState = Loading }


{-| Return an empty filter
-}
emptyFilter : Filter
emptyFilter =
    ""


{-| Create a command to send the given messageHolder to the server.
-}
sendMessageToServer : MessageHolder.MessageHolder -> (Result Http.Error (List MessageHolder.MessageHolder) -> msg) -> Cmd msg
sendMessageToServer messageHolder resultFunction =
    Http.post
        { url = serverUrl
        , body = encodeMessageHolder messageHolder |> Http.jsonBody
        , expect = Http.expectJson resultFunction decodeMessageHolderList
        }


{-| Create a command to request all serverMessages
-}
requestServerMessages : (Result Http.Error (List MessageHolder.MessageHolder) -> msg) -> Cmd msg
requestServerMessages resultFunction =
    Http.get
        { url = serverUrl
        , expect = Http.expectJson resultFunction decodeMessageHolderList
        }


{-| Function that handles the result of the server requests and return the parsed ServerMessageHolder
-}
handleServerResponse : ServerMessageHolder -> Result err (List MessageHolder.MessageHolder) -> ServerMessageHolder
handleServerResponse serverMessageHolder result =
    case result of
        Ok val ->
            { serverMessageHolder | serverState = MessageHolderList val }

        Err _ ->
            { serverMessageHolder | serverState = Error }


{-| Set the State of the messageHolder
-}
setState : ServerMessageHolder -> ServerState -> ServerMessageHolder
setState serverMessageHolder serverState =
    { serverMessageHolder | serverState = serverState }



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Url to request the messages
-}
serverUrl : String
serverUrl =
    "http://shauck.ddns.net:8080/ss19_enigma_server/api/messages"


{-| Decoder for a list of MessageHolder objects
-}
decodeMessageHolderList : Json.Decode.Decoder (List MessageHolder.MessageHolder)
decodeMessageHolderList =
    Json.Decode.map6
        MessageHolder.MessageHolder
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "rawInput" Json.Decode.string)
        (Json.Decode.field "processedInput" Json.Decode.string)
        (Json.Decode.field "processedOutput" Json.Decode.string)
        (Json.Decode.field "foreignCharOption" Json.Decode.int
            |> Json.Decode.andThen
                (\val ->
                    if val == 0 then
                        Json.Decode.succeed MessageHolder.Include

                    else
                        Json.Decode.succeed MessageHolder.Ignore
                )
        )
        (Json.Decode.succeed MessageHolder.defaultConfig)
        |> Json.Decode.list


{-| Encode a MessageHolder as a json message
-}
encodeMessageHolder : MessageHolder.MessageHolder -> Json.Encode.Value
encodeMessageHolder messageHolder =
    let
        list =
            [ ( "description", Json.Encode.string <| messageHolder.description )
            , ( "rawInput", Json.Encode.string <| messageHolder.rawInput )
            , ( "processedInput", Json.Encode.string <| messageHolder.processedInput )
            , ( "processedOutput", Json.Encode.string <| messageHolder.processedOutput )
            , ( "foreignCharOption"
              , Json.Encode.int <|
                    case messageHolder.foreignCharOption of
                        MessageHolder.Include ->
                            0

                        MessageHolder.Ignore ->
                            1
              )
            ]
    in
    Json.Encode.object list
