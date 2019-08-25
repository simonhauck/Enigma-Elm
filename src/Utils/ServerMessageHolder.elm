module Utils.ServerMessageHolder exposing
    ( ServerMessageHolder(..)
    , defaultServerMessageHolder
    , handleServerResponse
    , requestServerMessages
    , sendMessageToServer
    )

import Http
import Json.Decode
import Json.Encode
import Utils.MessageHolder


type ServerMessageHolder
    = Loading
    | MessageHolderList (List Utils.MessageHolder.MessageHolder)
    | Error



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


defaultServerMessageHolder : ServerMessageHolder
defaultServerMessageHolder =
    Loading


sendMessageToServer : Utils.MessageHolder.MessageHolder -> (Result Http.Error (List Utils.MessageHolder.MessageHolder) -> msg) -> Cmd msg
sendMessageToServer messageHolder resultFunction =
    Http.post
        { url = serverUrl
        , body = encodeMessageHolder messageHolder |> Http.jsonBody
        , expect = Http.expectJson resultFunction decodeMessageHolderList
        }


requestServerMessages : (Result Http.Error (List Utils.MessageHolder.MessageHolder) -> msg) -> Cmd msg
requestServerMessages resultFunction =
    Http.get
        { url = serverUrl
        , expect = Http.expectJson resultFunction decodeMessageHolderList
        }


handleServerResponse : Result err (List Utils.MessageHolder.MessageHolder) -> ServerMessageHolder
handleServerResponse result =
    case result of
        Ok val ->
            MessageHolderList val

        Err _ ->
            Error



--handleServerResponse
-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


serverUrl : String
serverUrl =
    "http://shauck.ddns.net:8080/ss19_enigma_server/api/messages"


decodeMessageHolderList : Json.Decode.Decoder (List Utils.MessageHolder.MessageHolder)
decodeMessageHolderList =
    Json.Decode.map5
        Utils.MessageHolder.MessageHolder
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "rawInput" Json.Decode.string)
        (Json.Decode.field "processedInput" Json.Decode.string)
        (Json.Decode.field "processedOutput" Json.Decode.string)
        (Json.Decode.field "foreignCharOption" Json.Decode.int
            |> Json.Decode.andThen
                (\val ->
                    if val == 0 then
                        Json.Decode.succeed Utils.MessageHolder.Include

                    else
                        Json.Decode.succeed Utils.MessageHolder.Ignore
                )
        )
        |> Json.Decode.list


encodeMessageHolder : Utils.MessageHolder.MessageHolder -> Json.Encode.Value
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
                        Utils.MessageHolder.Include ->
                            0

                        Utils.MessageHolder.Ignore ->
                            1
              )
            ]
    in
    Json.Encode.object list
