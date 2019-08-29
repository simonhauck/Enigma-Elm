module View.StyleElements exposing
    ( backgroundImage
    , buttonStyleElements
    , fontColor
    , fontFamily
    , h2StyleElements
    , h3StyleElements
    , input
    , plugboardButtonStyleElements
    , selectStyleElements
    , selectWrapperStyleElements
    , textarea
    )

import Html
import Html.Attributes



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


textarea : List (Html.Attribute msg)
textarea =
    [ Html.Attributes.style "border-radius" "10px", Html.Attributes.style "width" "300px", Html.Attributes.style "height" "80px", Html.Attributes.style "padding" "10px" ]


input : List (Html.Attribute msg)
input =
    [ Html.Attributes.style "border-radius" "10px", Html.Attributes.style "padding" "10px", Html.Attributes.style "border-width" "0px" ]


fontFamily : List (Html.Attribute msg)
fontFamily =
    [ Html.Attributes.style "font-family" "Arial" ]


fontColor : List (Html.Attribute msg)
fontColor =
    [ Html.Attributes.style "color" "#ffffff" ]


backgroundImage : List (Html.Attribute msg)
backgroundImage =
    [ Html.Attributes.style "background" "url(images/chalkboard.jpg) no-repeat center center fixed"
    , Html.Attributes.style "background-size" "cover"
    ]


selectWrapperStyleElements : List (Html.Attribute msg)
selectWrapperStyleElements =
    --    [ Html.Attributes.width selectWrapperWidth ]
    []


selectStyleElements : List (Html.Attribute msg)
selectStyleElements =
    [ Html.Attributes.style "padding"
        "7px"
    , Html.Attributes.style
        "font-size"
        "14px"
    ]



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


buttonStyleElements : List (Html.Attribute msg)
buttonStyleElements =
    [ Html.Attributes.style "background-color" "#cccecf"
    , Html.Attributes.style "border-radius" "10px"
    , Html.Attributes.style "margin" "5px"
    , Html.Attributes.style "font-size" "15px"
    , Html.Attributes.style "border-width" "0px"
    , Html.Attributes.style "padding" "10px"
    ]


plugboardButtonStyleElements : List (Html.Attribute msg)
plugboardButtonStyleElements =
    [ Html.Attributes.style "background-color" "#1ab87b"
    , Html.Attributes.style "border-radius" "5px"
    , Html.Attributes.style "border-width" "0px"
    , Html.Attributes.style "font-size" "18px"
    ]
        ++ fontColor
        ++ fontFamily


h3StyleElements : List (Html.Attribute msg)
h3StyleElements =
    [ Html.Attributes.style "font-weight" "normal" ] ++ fontFamily ++ fontColor


h2StyleElements : List (Html.Attribute msg)
h2StyleElements =
    [ Html.Attributes.style "font-weight" "normal" ] ++ fontFamily ++ fontColor



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


selectWrapperWidth : Int
selectWrapperWidth =
    200
