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
import Models.MessageHolder as MessageHolder
import Models.ServerMessageHolder as ServerMessageHolder
import View.StyleElements


type alias ConvertMessageHolderMsg msg =
    ServerMessageHolderMsg -> msg


type ServerMessageHolderMsg
    = SetMessageHolder MessageHolder.MessageHolder
    | SetServerMessageHolder ServerMessageHolder.ServerMessageHolder
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
    Html.div
        [ Html.Attributes.style "align-self" "stretch"
        , Html.Attributes.style "flex" "1"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "stretch"
        ]
        [ Html.div
            [ Html.Attributes.style "flex" "1"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "align-items" "stretch"
            ]
            [ Html.h2 (View.StyleElements.h2StyleElements ++ [ Html.Attributes.style "padding" "20px" ]) [ Html.text "Server Messages" ]
            , Html.div
                [ Html.Attributes.style "background-color" "rgba(255,255,255,0.2)"
                , Html.Attributes.style "border-radius" "20px"
                , Html.Attributes.style "padding" "20px"
                , Html.Attributes.style "margin-right" "20px"
                , Html.Attributes.style "margin-top" "20px"
                , Html.Attributes.style "align-self" "stretch"
                ]
                [ displayServerMessageHolderTable serverMessageHolder convertMessageHolderFunction
                ]
            ]
        ]


displayEncryptionResult : MessageHolder.MessageHolder -> ConvertMessageHolderMsg msg -> Html msg
displayEncryptionResult messageHolder convertMessageHolderFunction =
    let
        ( formattedInput, formattedOutput ) =
            MessageHolder.getFormattedProcessedInputOutput messageHolder
    in
    Html.div
        []
        [ Html.h3 View.StyleElements.h3StyleElements [ Html.text "Encryption Results" ]
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
                        MessageHolder.setDescription messageHolder val |> SetMessageHolder |> convertMessageHolderFunction
                    )
                ]
                []
            , Html.button
                ((Html.Events.onClick <| convertMessageHolderFunction <| SendMessageToServer)
                    :: Html.Attributes.disabled (String.isEmpty messageHolder.description || String.isEmpty messageHolder.processedOutput)
                    :: View.StyleElements.buttonStyleElements
                )
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

        SetServerMessageHolder newServerMessageHolder ->
            ( newServerMessageHolder, messageHolder, Cmd.none )

        StartLoadingServerMessages ->
            ( { serverMessageHolder | serverState = ServerMessageHolder.Loading }
            , messageHolder
            , (ResultLoadingServerMessages >> convertMessageFunction) |> ServerMessageHolder.requestServerMessages
            )

        SelectServerMessage selectedMessageHolder ->
            ( serverMessageHolder
            , MessageHolder.copyConfig messageHolder selectedMessageHolder |> MessageHolder.disableAutomaticEncryptionMode
            , Cmd.none
            )

        SendMessageToServer ->
            ( { serverMessageHolder | serverState = ServerMessageHolder.Loading }
            , MessageHolder.defaultMessageHolder
            , (ResultLoadingServerMessages >> convertMessageFunction) |> ServerMessageHolder.sendMessageToServer messageHolder
            )

        ResultLoadingServerMessages result ->
            ( ServerMessageHolder.handleServerResponse serverMessageHolder result
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
            case serverMessageHolder.serverState of
                ServerMessageHolder.Loading ->
                    displayLoadingBar

                ServerMessageHolder.Error ->
                    displayServerError

                ServerMessageHolder.MessageHolderList list ->
                    List.indexedMap (displayServerMessageRow convertMessageHolderFunction serverMessageHolder.filter) list
    in
    Html.table
        View.StyleElements.fontFamily
        (Html.tr
            []
            [ Html.td [] [ Html.text "Index" ]
            , Html.td [] [ Html.text "Description" ]
            , Html.td []
                [ Html.input
                    ([ Html.Attributes.type_ "text"
                     , Html.Attributes.placeholder "Filter Description..."
                     , Html.Attributes.value serverMessageHolder.filter
                     , Html.Events.onInput
                        (\val ->
                            { serverMessageHolder | filter = val } |> SetServerMessageHolder |> convertMessageHolderFunction
                        )
                     ]
                        ++ View.StyleElements.input
                    )
                    []
                ]
            , Html.td [] [ Html.text "RawInput" ]
            , Html.td []
                [ Html.button
                    ((Html.Events.onClick <| convertMessageHolderFunction StartLoadingServerMessages) :: View.StyleElements.buttonStyleElements)
                    [ Html.text "Reload" ]
                ]
            ]
            :: displayedItemList
        )


displayServerMessageRow : ConvertMessageHolderMsg msg -> ServerMessageHolder.Filter -> Int -> MessageHolder.MessageHolder -> Html msg
displayServerMessageRow convertMessageHolderFunction filter index messageHolder =
    let
        cleanString =
            \inputString -> String.trim inputString |> String.toLower |> String.toLower |> String.replace " " ""

        visibility =
            if cleanString messageHolder.description |> String.contains (cleanString filter) then
                "visible"

            else
                "collapse"
    in
    Html.tr
        [ Html.Attributes.style "visibility" visibility ]
        [ Html.td [] [ index |> String.fromInt |> Html.text ]
        , Html.td [] [ messageHolder.description |> Html.text ]
        , Html.td [] [ messageHolder.rawInput |> String.slice 0 20 |> Html.text ]
        , Html.td []
            [ Html.button
                ((Html.Events.onClick <| (convertMessageHolderFunction <| SelectServerMessage messageHolder)) :: View.StyleElements.buttonStyleElements)
                [ Html.text "Use" ]
            ]
        ]


displayLoadingBar : List (Html msg)
displayLoadingBar =
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


displayServerError : List (Html msg)
displayServerError =
    [ Html.tr [] [ Html.td [ Html.Attributes.colspan 4 ] [ Html.text "An error occurred while connecting to the server" ] ] ]
