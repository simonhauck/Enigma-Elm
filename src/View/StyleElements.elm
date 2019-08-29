module View.StyleElements exposing
    ( backgroundImage
    , buttonStyleElements
    , h2StyleElements
    , h3StyleElements
    , plugboardButtonStyleElements
    , selectStyleElements
    , selectWrapperStyleElements
    )

import Html
import Html.Attributes



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


backgroundImage : Html.Attribute msg
backgroundImage =
    --    Html.Attributes.style "background-color" "#272727"
    Html.Attributes.style "background-color" "fff"


selectWrapperStyleElements : List (Html.Attribute msg)
selectWrapperStyleElements =
    --    [ Html.Attributes.width selectWrapperWidth ]
    []


selectStyleElements : List (Html.Attribute msg)
selectStyleElements =
    --    [ Html.Attributes.style "-moz-appearance" "none"
    --    , Html.Attributes.style "-webkit-appearance" "none"
    --    , Html.Attributes.style "appearance" "none"
    --    , Html.Attributes.style "border" "none"
    --    , Html.Attributes.style "width" "100%"
    --    , Html.Attributes.style "height" "40px"
    --    , Html.Attributes.style "padding-left" "10px"
    --    , Html.Attributes.style "color" "#FFE400"
    --    , Html.Attributes.style "font-size" "16px"
    --    , Html.Attributes.style "border-radius" "3px"
    --    , Html.Attributes.style "cursor" "pointer"
    --    , Html.Attributes.style "background-color" "#747474"
    --   TODO Not working , Html.Attributes.style "font-family" "'Open Sans', sans-serif"
    --    ]
    [ Html.Attributes.style "background-color" "#747474" ]


buttonStyleElements : List (Html.Attribute msg)
buttonStyleElements =
    [ Html.Attributes.style "background-color" "black" ]


plugboardButtonStyleElements : List (Html.Attribute msg)
plugboardButtonStyleElements =
    [ Html.Attributes.style "background-color" "#747474" ]


h3StyleElements : List (Html.Attribute msg)
h3StyleElements =
    [ Html.Attributes.style "font-size" "16px" ]


h2StyleElements : List (Html.Attribute msg)
h2StyleElements =
    [ Html.Attributes.style "font-size" "30px" ]



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


selectWrapperWidth : Int
selectWrapperWidth =
    200
