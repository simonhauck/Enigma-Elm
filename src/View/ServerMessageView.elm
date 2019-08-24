module View.ServerMessageView exposing (displayServerMessages)

import Html exposing (Html)
import Html.Events
import Utils.MessageHolder as MessageHolder
import Utils.ServerMessageHolder



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------
--TODO ETA Reduction


displayServerMessages : (MessageHolder.MessageHolder -> msg) -> Utils.ServerMessageHolder.ServerMessageHolder -> Html msg
displayServerMessages onClickFunction serverMessageHolder =
    displayServerMessageHolderTable onClickFunction serverMessageHolder



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


displayServerMessageHolderTable : (MessageHolder.MessageHolder -> msg) -> Utils.ServerMessageHolder.ServerMessageHolder -> Html msg
displayServerMessageHolderTable onClickFunction serverMessageHolder =
    Html.table
        []
        (Html.tr
            []
            [ Html.td [] [ Html.text "Index" ], Html.td [] [ Html.text "Description" ], Html.td [] [ Html.text "RawInput" ] ]
            :: List.indexedMap (displayServerMessageRow onClickFunction) serverMessageHolder
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
