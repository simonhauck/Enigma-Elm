module View.StyleElements exposing (backgroundImage, selectStyleElements, selectWrapperStyleElements)

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
    []



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


selectWrapperWidth : Int
selectWrapperWidth =
    200
