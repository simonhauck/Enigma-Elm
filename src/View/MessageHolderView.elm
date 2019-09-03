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
import Model exposing (Model)
import Models.Enigma.EnigmaMachine
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


{-| Display the server messages in an table
-}
displayServerMessages : ServerMessageHolder.ServerMessageHolder -> ConvertMessageHolderMsg msg -> Html msg
displayServerMessages serverMessageHolder convertMessageHolderFunction =
    Html.div
        [ View.StyleElements.flexDirectionColumn ]
        [ Html.h2 View.StyleElements.h2StyleElements [ Html.text "Server Messages" ]
        , Html.div
            [ Html.Attributes.style "max-height" "600px"
            , View.StyleElements.smallMargin
            , View.StyleElements.flexDisplay
            ]
            [ Html.div
                ([ Html.Attributes.style "overflow" "auto", Html.Attributes.style "width" "100%" ] ++ View.StyleElements.smallElementBox)
                [ displayServerMessageHolderTable serverMessageHolder convertMessageHolderFunction ]
            ]
        ]


{-| Display the encryptionResults and a textField/button to send the message to the server
-}
displayEncryptionResult : MessageHolder.MessageHolder -> ConvertMessageHolderMsg msg -> Html msg
displayEncryptionResult messageHolder convertMessageHolderFunction =
    let
        ( formattedInput, formattedOutput ) =
            MessageHolder.getFormattedProcessedInputOutput messageHolder
    in
    Html.div
        ([ View.StyleElements.smallMargin, Html.Attributes.style "width" "fit-content" ] ++ View.StyleElements.smallElementBox)
        [ Html.h3 View.StyleElements.h3StyleElements [ Html.text "Encryption Results" ]
        , Html.div
            []
            [ Html.table
                View.StyleElements.messageHolderTable
                [ Html.tr
                    [ View.StyleElements.smallMargin ]
                    [ Html.td
                        ([ Html.Attributes.style "width" "160px"
                         , Html.Attributes.style "vertical-align" "top"
                         ]
                            ++ View.StyleElements.monoSpaceText
                        )
                        [ Html.text "Processed Input : " ]
                    , Html.td ([ Html.Attributes.style "width" "auto" ] ++ View.StyleElements.monoSpaceText) [ Html.text formattedInput ]
                    ]
                , Html.tr
                    [ View.StyleElements.smallMargin ]
                    [ Html.td
                        ([ Html.Attributes.style "width" "160px"
                         , Html.Attributes.style "vertical-align" "top"
                         ]
                            ++ View.StyleElements.monoSpaceText
                        )
                        [ Html.text "Processed Output: " ]
                    , Html.td ([ Html.Attributes.style "width" "auto" ] ++ View.StyleElements.monoSpaceText) [ Html.text formattedOutput ]
                    ]
                ]
            , Html.input
                ([ Html.Attributes.type_ "text"
                 , Html.Attributes.placeholder "Add a description..."
                 , Html.Attributes.value messageHolder.description
                 , Html.Attributes.style "width" "200px"
                 , Html.Events.onInput
                    (\val ->
                        MessageHolder.setDescription messageHolder val |> SetMessageHolder |> convertMessageHolderFunction
                    )
                 ]
                    ++ View.StyleElements.input
                )
                []
            , Html.button
                ((Html.Events.onClick <| convertMessageHolderFunction <| SendMessageToServer)
                    :: Html.Attributes.disabled (String.isEmpty messageHolder.description || String.isEmpty messageHolder.processedOutput)
                    :: View.StyleElements.buttonStyleElements
                )
                [ Html.text "Send message to server" ]
            ]
        ]


update : ServerMessageHolderMsg -> Model -> ConvertMessageHolderMsg msg -> ( Model, Cmd msg )
update serverMessageHolderMsg model convertMessageFunction =
    case serverMessageHolderMsg of
        SetMessageHolder newMessageHolder ->
            ( { model | messageHolder = newMessageHolder }, Cmd.none )

        SetServerMessageHolder newServerMessageHolder ->
            ( { model | serverMessageHolder = newServerMessageHolder }, Cmd.none )

        StartLoadingServerMessages ->
            ( { model | serverMessageHolder = ServerMessageHolder.setState model.serverMessageHolder ServerMessageHolder.Loading }
            , (ResultLoadingServerMessages >> convertMessageFunction) |> ServerMessageHolder.requestServerMessages
            )

        SelectServerMessage selectedMessageHolder ->
            ( { model
                | enigma = Models.Enigma.EnigmaMachine.setStartPositionAsCurrentPosition model.enigma
                , messageHolder =
                    MessageHolder.copyConfig model.messageHolder selectedMessageHolder
                        |> MessageHolder.disableAutomaticEncryptionMode
                , substitutionLog = Nothing
              }
            , Cmd.none
            )

        SendMessageToServer ->
            ( { model
                | serverMessageHolder = ServerMessageHolder.setState model.serverMessageHolder ServerMessageHolder.Loading
                , messageHolder = MessageHolder.defaultMessageHolder
              }
            , (ResultLoadingServerMessages >> convertMessageFunction) |> ServerMessageHolder.sendMessageToServer model.messageHolder
            )

        ResultLoadingServerMessages result ->
            ( { model | serverMessageHolder = ServerMessageHolder.handleServerResponse model.serverMessageHolder result }
            , Cmd.none
            )



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


{-| Display the ServerMessageHolder in a table
-}
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
        [ View.StyleElements.fontFamily
        , View.StyleElements.fontColor
        ]
        ([ Html.tr
            []
            [ Html.th [ Html.Attributes.style "width" "5%", Html.Attributes.style "text-align" "center" ] [ Html.text "#" ]
            , Html.th [ Html.Attributes.style "width" "45%" ] [ Html.text "Description" ]
            , Html.th [ Html.Attributes.style "width" "40%" ] [ Html.text "RawInput" ]
            , Html.th [ Html.Attributes.style "width" "10%", Html.Attributes.style "text-align" "center" ]
                [ Html.button
                    ((Html.Events.onClick <| convertMessageHolderFunction StartLoadingServerMessages) :: View.StyleElements.buttonStyleElements)
                    [ Html.text "Reload" ]
                ]
            ]
         , Html.tr
            []
            [ Html.th [] []
            , Html.th []
                [ Html.input
                    ([ Html.Attributes.type_ "text"
                     , Html.Attributes.placeholder "Filter Description..."
                     , Html.Attributes.value serverMessageHolder.filter
                     , Html.Attributes.style "width" "-webkit-fill-available"
                     , Html.Events.onInput
                        (\val ->
                            { serverMessageHolder | filter = val } |> SetServerMessageHolder |> convertMessageHolderFunction
                        )
                     ]
                        ++ View.StyleElements.input
                    )
                    []
                ]
            , Html.th [] []
            , Html.th [] []
            ]
         ]
            ++ displayedItemList
        )


{-| Display a row for one messageHolder in the table.
convertMessageHolderFunction - convert a ServerMessageHolderMsg to a msg
filter - a filter for the description. Only elements matching this filter will be displayed
index - the index of the component
messageHolder - that should be displayed
-}
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
        [ Html.td
            [ View.StyleElements.mediumPaddingRight ]
            [ index |> String.fromInt |> Html.text ]
        , Html.td
            ([ View.StyleElements.mediumPaddingRight ] ++ View.StyleElements.serverMessageColumn)
            [ messageHolder.description |> Html.text ]
        , Html.td
            ([ View.StyleElements.mediumPaddingRight ] ++ View.StyleElements.serverMessageColumn)
            [ messageHolder.rawInput |> MessageHolder.getFormattedText messageHolder |> Html.text ]
        , Html.td
            [ Html.Attributes.style "text-align" "center" ]
            [ Html.button
                ([ Html.Events.onClick <| convertMessageHolderFunction <| SelectServerMessage messageHolder
                 ]
                    ++ View.StyleElements.buttonStyleElements
                )
                [ Html.text "Use" ]
            ]
        ]


{-| Display a loading bar in a row
-}
displayLoadingBar : List (Html msg)
displayLoadingBar =
    [ Html.tr
        []
        [ Html.td
            [ Html.Attributes.colspan 4 ]
            [ Loading.render Loading.Bars
                { defaultConfig | color = "#fff" }
                Loading.On
            ]
        ]
    ]


{-| Display an error message in a row
-}
displayServerError : List (Html msg)
displayServerError =
    [ Html.tr [] [ Html.td [ Html.Attributes.colspan 4 ] [ Html.text "An error occurred while connecting to the server" ] ] ]
