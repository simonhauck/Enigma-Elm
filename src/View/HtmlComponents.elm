module View.HtmlComponents exposing (checkBox, rangeSlider)

import Html exposing (Html)
import Html.Attributes
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
    Html.Styled.toUnstyled <|
        Html.Styled.label
            View.StyleElements.checkBoxLabel
            [ Html.Styled.input
                ([ Html.Styled.Attributes.type_ "checkBox" ] ++ View.StyleElements.checkBoxInput)
                []
            , Html.Styled.span
                View.StyleElements.checkBoxSpan
                []
            ]
