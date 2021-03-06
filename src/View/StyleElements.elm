module View.StyleElements exposing
    ( backgroundImage
    , buttonStyleElements
    , checkBoxSpan
    , configurationFirstColumnWidth
    , customInput
    , customLabel
    , fillSvg
    , flexDirectionColumn
    , flexDisplay
    , flexGrow
    , fontColor
    , fontFamily
    , fontFamilySvg
    , fontSizeText
    , fourthColor
    , h1StyleElements
    , h2StyleElements
    , h3StyleElements
    , input
    , mediumMargin
    , mediumPaddingRight
    , messageHolderTable
    , monoSpaceText
    , plugboardButtonStyleElements
    , primaryColor
    , radioSpan
    , rangeSlider
    , secondaryColor
    , selectStyleElements
    , selectWrapperStyleElements
    , serverMessageColumn
    , smallElementBox
    , smallMargin
    , textarea
    , thirdColor
    )

import Css
import Css.Global
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
    [ Html.Attributes.style "background" "url(images/wheelDark.jpg) "
    , Html.Attributes.style "background-size" "100% 100%"
    , Html.Attributes.style "background-repeat" "no-repeat"
    , Html.Attributes.style "overflow-x" "auto"
    , Html.Attributes.style "background-attachment" "fixed"
    ]


primaryColor : String
primaryColor =
    "#00ffea"


secondaryColor : String
secondaryColor =
    "#828282"


thirdColor : String
thirdColor =
    "#d6d6d6"


fourthColor : String
fourthColor =
    "#ffa10f"


fifthColor : String
fifthColor =
    "#619bff"


flexDisplay : Html.Attribute msg
flexDisplay =
    Html.Attributes.style "display" "flex"


flexDirectionColumn : Html.Attribute msg
flexDirectionColumn =
    Html.Attributes.style "flex-direction" "column"


flexGrow : Html.Attribute msg
flexGrow =
    Html.Attributes.style "flex-grow" "1"


smallElementBox : List (Html.Attribute msg)
smallElementBox =
    [ Html.Attributes.style "background-color" "rgba(255,255,255,0.2)"
    , Html.Attributes.style "border-radius" "20px"
    , Html.Attributes.style "padding" "10px"
    , Html.Attributes.style "margin-top" "15px"
    , Html.Attributes.style "margin-bottom" "15px"
    ]


fontFamily : Html.Attribute msg
fontFamily =
    Html.Attributes.style "font-family" "Arial"


fontFamilySvg : Svg.Attribute msg
fontFamilySvg =
    Svg.Attributes.fontFamily "monospace"


monoSpaceText : List (Html.Attribute msg)
monoSpaceText =
    [ Html.Attributes.style "font-family" "monospace"
    , Html.Attributes.style "font-size" "15px"
    , Html.Attributes.style "overflow-wrap" "break-word"
    , Html.Attributes.style "word-wrap" "break-word"
    , Html.Attributes.style "word-break" "break-word"
    , fontColor
    ]


fontColor : Html.Attribute msg
fontColor =
    Html.Attributes.style "color" "#ffffff"


fontSizeText : Html.Attribute msg
fontSizeText =
    Html.Attributes.style "font-size" "15px"


fillSvg : Svg.Attribute msg
fillSvg =
    Svg.Attributes.fill primaryColor


smallMargin : Html.Attribute msg
smallMargin =
    Html.Attributes.style "margin" "5px"


mediumMargin : Html.Attribute msg
mediumMargin =
    Html.Attributes.style "margin" "10px"


mediumPaddingRight : Html.Attribute msg
mediumPaddingRight =
    Html.Attributes.style "padding-right" "10px"


h1StyleElements : List (Html.Attribute msg)
h1StyleElements =
    [ fontColor
    , Html.Attributes.style "text-align" "center"
    , Html.Attributes.style "font-size" "350%"
    ]


h2StyleElements : List (Html.Attribute msg)
h2StyleElements =
    [ Html.Attributes.style "font-weight" "normal"
    , Html.Attributes.style "padding-left" "5px"
    , Html.Attributes.style "text-align" "center"
    , fontFamily
    , fontColor
    ]


h3StyleElements : List (Html.Attribute msg)
h3StyleElements =
    [ Html.Attributes.style "font-weight" "normal"
    , fontFamily
    , fontColor
    ]


buttonStyleElements : List (Html.Attribute msg)
buttonStyleElements =
    [ Html.Attributes.style "background-color" "#cccecf"
    , Html.Attributes.style "border-radius" "10px"
    , Html.Attributes.style "margin" "5px"
    , Html.Attributes.style "font-size" "15px"
    , Html.Attributes.style "border-width" "0px"
    , Html.Attributes.style "padding" "10px"
    ]


{-| Elm cant define css attributes like hover -> So use library
-}
plugboardButtonStyleElements : Html.Styled.Attribute msg
plugboardButtonStyleElements =
    Html.Styled.Attributes.css
        [ Css.property "background-color" secondaryColor
        , Css.property "border-radius" "5px"
        , Css.property "border-width" "0px"
        , Css.property "font-size" "18px"
        , Css.property "color" "#ffffff"
        , Css.property "font-family" "Arial"
        , Css.property "transition" "opacity .1s"
        , Css.disabled
            [ Css.property "background-color" "#404040"
            , Css.property "color" "#a3a399"
            ]
        , Css.enabled
            [ Css.hover
                [ Css.property "background-color" "#969696" ]
            ]
        ]


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


serverMessageColumn : List (Html.Attribute msg)
serverMessageColumn =
    [ Html.Attributes.style "max-width" "0"
    , Html.Attributes.style "overflow" "hidden"
    , Html.Attributes.style "text-overflow" "ellipsis"
    , Html.Attributes.style "white-space" "nowrap"
    ]


configurationFirstColumnWidth : Html.Attribute msg
configurationFirstColumnWidth =
    Html.Attributes.style "min-width" "120px"


{-| Elm cant define css attributes like hover -> So use library
-}
rangeSlider : List (Html.Styled.Attribute msg)
rangeSlider =
    [ Html.Styled.Attributes.css
        [ Css.disabled [ Css.property "background" "#EBEBE4" ]
        , Css.enabled [ Css.hover [ Css.property "opacity" "1" ] ]
        , Css.pseudoElement "-webkit-slider-thumb"
            [ Css.disabled
                [ Css.property "background" "#a3a399" ]
            , Css.property "-webkit-appearance" "none"
            , Css.property "appearance" "none"
            , Css.property "width" "25px"
            , Css.property "height" "25px"
            , Css.property "border-radius" "50%"
            , Css.property "background" fifthColor
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


{-| Elm cant define css attributes like hover -> So use library
-}
customLabel : List (Html.Styled.Attribute msg)
customLabel =
    [ Html.Styled.Attributes.css
        [ Css.property "display" "block"
        , Css.property "position" "relative"
        , Css.property "padding-left" "25px"
        , Css.property "margin-bottom" "12px"
        , Css.property "margin" "5px"
        , Css.property "cursor" "pointer"
        , Css.property "font-size" "22px"
        , Css.property "-webkit-user-select" "none"
        , Css.property "-moz-user-select" "none"
        , Css.property "-ms-user-select" "none"
        , Css.property "user-select" "none"
        ]
    ]


{-| Elm cant define css attributes like hover -> So use library
-}
customInput : List (Html.Styled.Attribute msg)
customInput =
    [ Html.Styled.Attributes.css
        [ Css.checked
            [ Css.Global.generalSiblings
                [ Css.Global.typeSelector "span"
                    [ Css.property "background-color" fifthColor
                    , Css.after [ Css.property "display" "block" ]
                    ]
                ]
            ]
        , Css.disabled
            [ Css.Global.generalSiblings
                [ Css.Global.typeSelector "span"
                    [ Css.property "background" "#a3a399" ]
                ]
            ]
        , Css.property "position" "absolute"
        , Css.property "opacity" "0"
        , Css.property "cursor" "pointer"
        , Css.property "height" "0"
        , Css.property "width" "0"
        ]
    ]


{-| Elm cant define css attributes like hover -> So use library
-}
checkBoxSpan : List (Html.Styled.Attribute msg)
checkBoxSpan =
    [ Html.Styled.Attributes.css
        [ Css.hover [ Css.property "background-color" "#ccc" ]
        , Css.after checkSymbol
        , Css.property "position" "absolute"
        , Css.property "top" "0"
        , Css.property "left" "0"
        , Css.property "height" "20px"
        , Css.property "width" "20px"
        , Css.property "background-color" "#eee"
        ]
    ]


{-| Elm cant define css attributes like hover -> So use library
-}
radioSpan : List (Html.Styled.Attribute msg)
radioSpan =
    [ Html.Styled.Attributes.css
        [ Css.hover [ Css.property "background-color" "#ccc" ]
        , Css.after radioSymbol
        , Css.property "position" "absolute"
        , Css.property "top" "0"
        , Css.property "left" "0"
        , Css.property "height" "17px"
        , Css.property "width" "17px"
        , Css.property "background-color" "#eee"
        , Css.property "border-radius" "50%"
        ]
    ]



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------


checkSymbol : List Css.Style
checkSymbol =
    [ Css.property "display" "none"
    , Css.property "content" "\"\""
    , Css.property "position" "absolute"
    , Css.property "left" "7px"
    , Css.property "top" "3px"
    , Css.property "width" "4px"
    , Css.property "height" "8px"
    , Css.property "border" "solid white"
    , Css.property "border-width" "0 3px 3px 0"
    , Css.property "-webkit-transform" "rotate(45deg)"
    , Css.property "-ms-transform" "rotate(45deg)"
    , Css.property "transform" "rotate(45deg)"
    ]


radioSymbol : List Css.Style
radioSymbol =
    [ Css.property "display" "none"
    , Css.property "content" "\"\""
    , Css.property "position" "absolute"
    , Css.property "left" "6px"
    , Css.property "top" "6px"
    , Css.property "width" "5px"
    , Css.property "height" "5px"
    , Css.property "border-radius" "50%"
    , Css.property "background" "white"
    ]
