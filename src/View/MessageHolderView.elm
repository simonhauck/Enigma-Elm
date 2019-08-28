module View.MessageHolderView exposing (ConvertMessageHolderMsg, ServerMessageHolderMsg(..), displayServerMessages, textInputBoxView, update)

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


textInputBoxView : MessageHolder.MessageHolder -> OperationMode.OperationMode -> ConvertMessageHolderMsg msg -> Html msg
textInputBoxView messageHolder operationMode convertMessageHolderFunction =
    Html.div []
        [ Html.h3 [] [ Html.text "Text Input" ]
        , Html.textarea
            [ Html.Attributes.placeholder "Enter your text here"
            , Html.Attributes.value messageHolder.rawInput
            , Html.Events.onInput (\val -> MessageHolder.setRawInput messageHolder val |> SetMessageHolder |> convertMessageHolderFunction)
            ]
            []
        , Html.button
            [ Html.Events.onClick <| convertMessageHolderFunction <| SetMessageHolder <| MessageHolder.toggleEncryptionMode messageHolder
            , enableAttributeWhenInEncryption operationMode
            ]
            [ case messageHolder.config.encryptionMode of
                MessageHolder.Automatic ->
                    Html.text "Disable automatic encryption"

                MessageHolder.Manual ->
                    Html.text "Enable automatic encryption"
            ]
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "25"
                , Html.Attributes.max "1000"
                , Html.Attributes.value <| String.fromInt messageHolder.config.encryptionSpeed
                , Html.Attributes.step "25"
                , Html.Events.onInput
                    (\val ->
                        String.toInt val
                            |> Maybe.withDefault 250
                            |> MessageHolder.setEncryptionSpeed messageHolder
                            |> SetMessageHolder
                            |> convertMessageHolderFunction
                    )
                , enableAttributeWhenInEncryption operationMode
                ]
                []
            , Html.text ("Time between Ticks: " ++ String.fromInt messageHolder.config.encryptionSpeed)
            ]
        ]



--        TODO Remove
--SetEncryptionModeSpeed (Maybe.withDefault 250 (String.toInt val))


update : ServerMessageHolderMsg -> ServerMessageHolder.ServerMessageHolder -> MessageHolder.MessageHolder -> ConvertMessageHolderMsg msg -> ( ServerMessageHolder.ServerMessageHolder, MessageHolder.MessageHolder, Cmd msg )
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


enableAttributeWhenInEncryption : OperationMode.OperationMode -> Html.Attribute msg
enableAttributeWhenInEncryption operationMode =
    case operationMode of
        OperationMode.Configuration ->
            Html.Attributes.disabled True

        OperationMode.Encryption ->
            Html.Attributes.disabled False
