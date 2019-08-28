module View.ServerMessageView exposing (displayServerMessages)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Loading exposing (defaultConfig)
import Models.MessageHolder as MessageHolder
import Models.ServerMessageHolder as ServerMessageHolder



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------
--TODO ETA Reduction


displayServerMessages : (MessageHolder.MessageHolder -> msg) -> msg -> ServerMessageHolder.ServerMessageHolder -> Html msg
displayServerMessages useFunction reloadFunction serverMessageHolder =
    displayServerMessageHolderTable useFunction reloadFunction serverMessageHolder



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


displayServerMessageHolderTable : (MessageHolder.MessageHolder -> msg) -> msg -> ServerMessageHolder.ServerMessageHolder -> Html msg
displayServerMessageHolderTable useFunction reloadFunction serverMessageHolder =
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
                    List.indexedMap (displayServerMessageRow useFunction) list
    in
    Html.table
        []
        (Html.tr
            []
            [ Html.td [] [ Html.text "Index" ]
            , Html.td [] [ Html.text "Description" ]
            , Html.td [] [ Html.text "RawInput" ]
            , Html.td [] [ Html.button [ Html.Events.onClick reloadFunction ] [ Html.text "Reload" ] ]
            ]
            :: displayedItemList
        )


displayServerMessageRow : (MessageHolder.MessageHolder -> msg) -> Int -> MessageHolder.MessageHolder -> Html msg
displayServerMessageRow onClickFunction index messageHolder =
    Html.tr
        []
        [ Html.td [] [ index |> String.fromInt |> Html.text ]
        , Html.td [] [ messageHolder.description |> Html.text ]
        , Html.td [] [ messageHolder.rawInput |> String.slice 0 20 |> Html.text ]
        , Html.td [] [ Html.button [ Html.Events.onClick (onClickFunction messageHolder) ] [ Html.text "Use" ] ]
        ]
