module View.HtmlComponents exposing (rangeSlider, rangeSlider2)

import Html exposing (Html)
import Html.Attributes
import Html.Styled
import Html.Styled.Attributes
import View.StyleElements


rangeSlider : List (Html.Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes childElements =
    Html.div
        [ Html.Attributes.style "width" "100%" ]
        [ Html.input
            ([ Html.Attributes.type_ "range"
             ]
                ++ attributes
            )
            childElements
        ]


rangeSlider2 : List (Html.Attribute msg) -> List (Html msg) -> Html msg
rangeSlider2 attributes childElements =
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
