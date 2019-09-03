module View.HtmlComponents exposing (checkBox, plugboardButton, radioButton, rangeSlider)

import Html exposing (Html)
import Html.Styled
import Html.Styled.Attributes
import View.StyleElements


{-| Custom Range slider that can be modified the given attributes and a list of child elements
-}
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


{-| Custom Checkbox that can be modified the given attributes and a list of child elements
-}
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


{-| Custom Radiobutton that can be modified the given attributes and a list of child elements
-}
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


{-| Custom PlugboardButtons that can be modified the given attributes and a list of child elements
-}
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
