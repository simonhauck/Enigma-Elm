module View.MessageHolderView exposing
    ( ConvertMessageHolderMsg
    , ServerMessageHolderMsg(..)
    , displayEncryptionResult
    , displayServerMessages
    , update
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Loading exposing (defaultConfig)
import Models.Enigma.OperationMode as OperationMode
import Models.MessageHolder as MessageHolder
import Models.ServerMessageHolder as ServerMessageHolder


type alias ConvertMessageHolderMsg msg =
    ServerMessageHolderMsg -> msg


type ServerMessageHolderMsg
    = SetMessageHolder MessageHolder.MessageHolder
    | StartLoadingServerMessages
    | SelectServerMessage MessageHolder.MessageHolder
    | SendMessageToServer
    | ResultLoadingServerMessages (Result Http.Error (List MessageHolder.MessageHolder))



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------
--TODO ETA Reduction


displayServerMessages : ServerMessageHolder.ServerMessageHolder -> ConvertMessageHolderMsg msg -> Html msg
displayServerMessages serverMessageHolder convertMessageHolderFunction =
    displayServerMessageHolderTable serverMessageHolder convertMessageHolderFunction


displayEncryptionResult : MessageHolder.MessageHolder -> ConvertMessageHolderMsg msg -> Html msg
displayEncryptionResult messageHolder convertMessageHolderFunction =
    let
        ( formattedInput, formattedOutput ) =
            MessageHolder.getFormattedProcessedInputOutput messageHolder
    in
    Html.div
        []
        [ Html.h3 [] [ Html.text "Encryption Results" ]
        , Html.div
            []
            [ Html.table
                []
                [ Html.tr
                    []
                    [ Html.td [] [ Html.text "Processed Input: " ]
                    , Html.td [] [ Html.text formattedInput ]
                    ]
                , Html.tr
                    []
                    [ Html.td [] [ Html.text "Processed Output: " ]
                    , Html.td [] [ Html.text formattedOutput ]
                    ]
                ]
            , Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.placeholder "Add a description"
                , Html.Attributes.value messageHolder.description
                , Html.Events.onInput
                    (\val ->
                        { messageHolder | description = val } |> SetMessageHolder |> convertMessageHolderFunction
                    )
                ]
                []
            , Html.button
                [ Html.Events.onClick <| convertMessageHolderFunction <| SendMessageToServer
                , Html.Attributes.disabled (String.isEmpty messageHolder.description || String.isEmpty messageHolder.processedOutput)
                ]
                [ Html.text "Send message to server" ]
            ]
        ]


update :
    ServerMessageHolderMsg
    -> ServerMessageHolder.ServerMessageHolder
    -> MessageHolder.MessageHolder
    -> ConvertMessageHolderMsg msg
    -> ( ServerMessageHolder.ServerMessageHolder, MessageHolder.MessageHolder, Cmd msg )
update serverMessageHolderMsg serverMessageHolder messageHolder convertMessageFunction =
    case serverMessageHolderMsg of
        SetMessageHolder newMessageHolder ->
            ( serverMessageHolder, newMessageHolder, Cmd.none )

        StartLoadingServerMessages ->
            ( ServerMessageHolder.Loading
            , messageHolder
            , ResultLoadingServerMessages >> convertMessageFunction |> ServerMessageHolder.requestServerMessages
            )

        SelectServerMessage selectedMessageHolder ->
            ( serverMessageHolder
            , MessageHolder.copyConfig messageHolder selectedMessageHolder |> MessageHolder.disableAutomaticEncryptionMode
            , Cmd.none
            )

        SendMessageToServer ->
            ( ServerMessageHolder.Loading
            , MessageHolder.defaultMessageHolder
            , (ResultLoadingServerMessages >> convertMessageFunction) |> ServerMessageHolder.sendMessageToServer messageHolder
            )

        ResultLoadingServerMessages result ->
            ( ServerMessageHolder.handleServerResponse result
            , messageHolder
            , Cmd.none
            )



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


displayServerMessageHolderTable : ServerMessageHolder.ServerMessageHolder -> ConvertMessageHolderMsg msg -> Html msg
displayServerMessageHolderTable serverMessageHolder convertMessageHolderFunction =
    let
        displayedItemList =
            case serverMessageHolder of
                ServerMessageHolder.Loading ->
                    [ Html.tr
                        []
                        [ Html.td
                            [ Html.Attributes.colspan 4 ]
                            [ Loading.render Loading.Bars
                                { defaultConfig | color = "#333" }
                                Loading.On
                            ]
                        ]
                    ]

                ServerMessageHolder.Error ->
                    [ Html.tr [] [ Html.td [ Html.Attributes.colspan 4 ] [ Html.text "An error occurred while connecting to the server" ] ] ]

                ServerMessageHolder.MessageHolderList list ->
                    List.indexedMap (displayServerMessageRow convertMessageHolderFunction) list
    in
    Html.table
        []
        (Html.tr
            []
            [ Html.td [] [ Html.text "Index" ]
            , Html.td [] [ Html.text "Description" ]
            , Html.td [] [ Html.text "RawInput" ]
            , Html.td [] [ Html.button [ Html.Events.onClick <| convertMessageHolderFunction StartLoadingServerMessages ] [ Html.text "Reload" ] ]
            ]
            :: displayedItemList
        )


displayServerMessageRow : ConvertMessageHolderMsg msg -> Int -> MessageHolder.MessageHolder -> Html msg
displayServerMessageRow convertMessageHolderFunction index messageHolder =
    Html.tr
        []
        [ Html.td [] [ index |> String.fromInt |> Html.text ]
        , Html.td [] [ messageHolder.description |> Html.text ]
        , Html.td [] [ messageHolder.rawInput |> String.slice 0 20 |> Html.text ]
        , Html.td [] [ Html.button [ Html.Events.onClick <| (convertMessageHolderFunction <| SelectServerMessage messageHolder) ] [ Html.text "Use" ] ]
        ]
