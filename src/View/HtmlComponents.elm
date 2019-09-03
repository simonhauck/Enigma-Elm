module View.HtmlComponents exposing (checkBox, plugboardButton, radioButton, rangeSlider)

import Html exposing (Html)
import Html.Styled
import Html.Styled.Attributes
import View.StyleElements


rangeSlider : List (Html.Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes childElements =
    let
        styledAttributes =
            List.map Html.Styled.Attributes.fromUnstyled attributes

        styledChildElements =
            List.map Html.Styled.fromUnstyled childElements
    in
    Html.Styled.toUnstyled <|
        Html.Styled.input
            ([ Html.Styled.Attributes.type_ "range" ] ++ styledAttributes ++ View.StyleElements.rangeSlider)
            styledChildElements


checkBox : List (Html.Attribute msg) -> List (Html msg) -> Html msg
checkBox attributes childElements =
    let
        styledAttributes =
            List.map Html.Styled.Attributes.fromUnstyled attributes

        styledChildElements =
            List.map Html.Styled.fromUnstyled childElements
    in
    Html.Styled.toUnstyled <|
        Html.Styled.label
            View.StyleElements.customLabel
            ([ Html.Styled.input
                ([ Html.Styled.Attributes.type_ "checkBox" ] ++ View.StyleElements.customInput ++ styledAttributes)
                []
             , Html.Styled.span
                View.StyleElements.checkBoxSpan
                []
             ]
                ++ styledChildElements
            )


radioButton : List (Html.Attribute msg) -> List (Html msg) -> Html msg
radioButton attributes childElements =
    let
        styledAttributes =
            List.map Html.Styled.Attributes.fromUnstyled attributes

        styledChildElements =
            List.map Html.Styled.fromUnstyled childElements
    in
    Html.Styled.toUnstyled <|
        Html.Styled.label
            View.StyleElements.customLabel
            ([ Html.Styled.input
                ([ Html.Styled.Attributes.type_ "radio" ] ++ View.StyleElements.customInput ++ styledAttributes)
                []
             , Html.Styled.span
                View.StyleElements.radioSpan
                []
             ]
                ++ styledChildElements
            )


plugboardButton : List (Html.Attribute msg) -> List (Html msg) -> Html msg
plugboardButton attributes childElements =
    let
        styledAttributes =
            List.map Html.Styled.Attributes.fromUnstyled attributes

        styledChildElements =
            List.map Html.Styled.fromUnstyled childElements
    in
    Html.Styled.toUnstyled <|
        Html.Styled.button
            (View.StyleElements.plugboardButtonStyleElements :: styledAttributes)
            styledChildElements
