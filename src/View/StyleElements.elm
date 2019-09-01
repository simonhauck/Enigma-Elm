module View.StyleElements exposing
    ( backgroundImage
    , buttonStyleElements
    , checkBoxInput
    , checkBoxLabel
    , checkBoxSpan
    , fillSvg
    , flexDirectionColumn
    , flexDisplay
    , flexGrow
    , fontColor
    , fontFamily
    , fontFamilySvg
    , fontSizeText
    , h2StyleElements
    , h3StyleElements
    , input
    , mediumMargin
    , mediumPaddingRight
    , messageHolderTable
    , monoSpaceText
    , plugboardButtonStyleElements
    , primaryColor
    , rangeSlider
    , secondaryColor
    , selectStyleElements
    , selectWrapperStyleElements
    , serverMessageColumn
    , smallElementBox
    , smallMargin
    , tableBorderBottom
    , textarea
    )

import Css
import Html
import Html.Attributes
import Html.Styled
import Html.Styled.Attributes
import Svg
import Svg.Attributes



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


backgroundImage : List (Html.Attribute msg)
backgroundImage =
    [ Html.Attributes.style "background" "url(images/chalkboard.jpg) "
    , Html.Attributes.style "background-size" "cover"
    , Html.Attributes.style "background-repeat" "no-repeat"
    , Html.Attributes.style "overflow-x" "auto"
    ]


primaryColor : String
primaryColor =
    "#1ab87b"


secondaryColor : String
secondaryColor =
    "#3c8c6e"


flexDisplay : List (Html.Attribute msg)
flexDisplay =
    [ Html.Attributes.style "display" "flex" ]


flexDirectionColumn : List (Html.Attribute msg)
flexDirectionColumn =
    [ Html.Attributes.style "flex-direction" "column" ]


flexGrow : List (Html.Attribute msg)
flexGrow =
    [ Html.Attributes.style "flex-grow" "1" ]


smallElementBox : List (Html.Attribute msg)
smallElementBox =
    [ Html.Attributes.style "background-color" "rgba(255,255,255,0.2)"
    , Html.Attributes.style "border-radius" "20px"
    , Html.Attributes.style "padding" "10px"
    , Html.Attributes.style "margin-top" "15px"
    , Html.Attributes.style "margin-bottom" "15px"
    ]


fontFamily : List (Html.Attribute msg)
fontFamily =
    [ Html.Attributes.style "font-family" "Arial" ]


fontFamilySvg : Svg.Attribute msg
fontFamilySvg =
    Svg.Attributes.fontFamily "Arial"


monoSpaceText : List (Html.Attribute msg)
monoSpaceText =
    [ Html.Attributes.style "font-family" "monospace"
    , Html.Attributes.style "font-size" "15px"
    , Html.Attributes.style "overflow-wrap" "break-word"
    , Html.Attributes.style "word-wrap" "break-word"
    , Html.Attributes.style "word-break" "break-word"
    ]
        ++ fontColor


fontColor : List (Html.Attribute msg)
fontColor =
    [ Html.Attributes.style "color" "#ffffff" ]


fontSizeText : List (Html.Attribute msg)
fontSizeText =
    [ Html.Attributes.style "font-size" "15px" ]


fillSvg : Svg.Attribute msg
fillSvg =
    Svg.Attributes.fill primaryColor


smallMargin : List (Html.Attribute msg)
smallMargin =
    [ Html.Attributes.style "margin" "5px" ]


mediumMargin : List (Html.Attribute msg)
mediumMargin =
    [ Html.Attributes.style "margin" "10px" ]


mediumPaddingRight : List (Html.Attribute msg)
mediumPaddingRight =
    [ Html.Attributes.style "padding-right" "10px" ]


h2StyleElements : List (Html.Attribute msg)
h2StyleElements =
    [ Html.Attributes.style "font-weight" "normal"
    , Html.Attributes.style "padding-left" "5px"
    , Html.Attributes.style "text-align" "center"
    ]
        ++ fontFamily
        ++ fontColor


h3StyleElements : List (Html.Attribute msg)
h3StyleElements =
    [ Html.Attributes.style "font-weight" "normal" ]
        ++ fontFamily
        ++ fontColor


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
    [ Html.Attributes.style "background-color" primaryColor
    , Html.Attributes.style "border-radius" "5px"
    , Html.Attributes.style "border-width" "0px"
    , Html.Attributes.style "font-size" "18px"
    ]
        ++ fontColor
        ++ fontFamily


textarea : List (Html.Attribute msg)
textarea =
    [ Html.Attributes.style "border-radius" "10px"
    , Html.Attributes.style "width" "95%"
    , Html.Attributes.style "height" "100px"
    , Html.Attributes.style "padding" "10px"
    ]


input : List (Html.Attribute msg)
input =
    [ Html.Attributes.style "border-radius" "10px"
    , Html.Attributes.style "padding" "10px"
    , Html.Attributes.style "border-width" "0px"
    ]


selectWrapperStyleElements : List (Html.Attribute msg)
selectWrapperStyleElements =
    --    [ Html.Attributes.width selectWrapperWidth ]
    []


selectStyleElements : List (Html.Attribute msg)
selectStyleElements =
    [ Html.Attributes.style "padding" "7px"
    , Html.Attributes.style "font-size" "14px"
    ]


messageHolderTable : List (Html.Attribute msg)
messageHolderTable =
    [ Html.Attributes.style "table-layout" "fixed"
    , Html.Attributes.style "width" "100%"
    , Html.Attributes.style "white-space" "pre-line"
    ]


tableBorderBottom : List (Html.Attribute msg)
tableBorderBottom =
    [ Html.Attributes.style "border-bottom" "1px solid white;" ]


serverMessageColumn : List (Html.Attribute msg)
serverMessageColumn =
    [ Html.Attributes.style "max-width" "0"
    , Html.Attributes.style "overflow" "hidden"
    , Html.Attributes.style "text-overflow" "ellipsis"
    , Html.Attributes.style "white-space" "nowrap"
    ]


{-| Elm cant define css attributes like hover -> So use library
-}
rangeSlider : List (Html.Styled.Attribute msg)
rangeSlider =
    [ Html.Styled.Attributes.css
        [ Css.hover
            [ Css.property "opacity" "1" ]
        , Css.pseudoElement "-webkit-slider-thumb"
            [ Css.property "-webkit-appearance" "none"
            , Css.property "appearance" "none"
            , Css.property "width" "25px"
            , Css.property "height" "25px"
            , Css.property "border-radius" "50%"
            , Css.property "background" "#4CAF50"
            , Css.property "cursor" "pointer"
            ]
        , Css.property "-webkit-appearance" "none"
        , Css.property "appearance" "none"
        , Css.property "width" "auto"
        , Css.property "height" "15px"
        , Css.property "border-radius" "5px"
        , Css.property "background" "#d3d3d3"
        , Css.property "outline" "none"
        , Css.property "opacity" "0.7"
        , Css.property "-webkit-transition" ".2s"
        , Css.property "transition" "opacity .2s"
        ]
    ]


checkBoxLabel : List (Html.Styled.Attribute msg)
checkBoxLabel =
    [ Html.Styled.Attributes.css
        [ Css.hover
            []
        , Css.property "display" "block"
        , Css.property "position" "relative"
        , Css.property "padding-left" "35px"
        , Css.property " margin-bottom" "12px"
        , Css.property "cursor" "pointer"
        , Css.property "font-size" "22px"
        , Css.property "-webkit-user-select" "none"
        , Css.property "-moz-user-select" "none"
        , Css.property "-ms-user-select" "none"
        , Css.property "user-select" "none"
        ]
    ]


checkBoxInput : List (Html.Styled.Attribute msg)
checkBoxInput =
    [ Html.Styled.Attributes.css
        [ Css.property "position" "absolute"
        , Css.property "opacity" "0"
        , Css.property "cursor" "pointer"
        , Css.property "height" "0"
        , Css.property "width" "0"
        ]
    ]


checkBoxSpan : List (Html.Styled.Attribute msg)
checkBoxSpan =
    [ Html.Styled.Attributes.css
        [ Css.property "position" "absolute"
        , Css.property "top" "0"
        , Css.property "left" "0"
        , Css.property "height" "25px"
        , Css.property "width" "25px"
        , Css.property "background-color" "#eee"
        ]
    ]



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------
