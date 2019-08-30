module View.StyleElements exposing
    ( backgroundImage
    , buttonStyleElements
    , fillSvg
    , flexDirectionColumn
    , flexDisplay
    , fontColor
    , fontFamily
    , fontFamilySvg
    , h2StyleElements
    , h3StyleElements
    , input
    , mediumMargin
    , messageHolderTable
    , monoSpaceText
    , plugboardButtonStyleElements
    , primaryColor
    , secondaryColor
    , selectStyleElements
    , selectWrapperStyleElements
    , smallElementBox
    , smallMargin
    , textarea
    )

import Html
import Html.Attributes
import Svg
import Svg.Attributes



-- ---------------------------------------------------------------------------------------------------------------------
-- Exposed functions
-- ---------------------------------------------------------------------------------------------------------------------


backgroundImage : List (Html.Attribute msg)
backgroundImage =
    [ Html.Attributes.style "background" "url(images/chalkboard.jpg) no-repeat center center fixed"
    , Html.Attributes.style "background-size" "cover"
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



--    []


fontColor : List (Html.Attribute msg)
fontColor =
    [ Html.Attributes.style "color" "#ffffff" ]


fillSvg : Svg.Attribute msg
fillSvg =
    Svg.Attributes.fill primaryColor


smallMargin : List (Html.Attribute msg)
smallMargin =
    [ Html.Attributes.style "margin" "5px" ]


mediumMargin : List (Html.Attribute msg)
mediumMargin =
    [ Html.Attributes.style "margin" "10px" ]


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
    , Html.Attributes.style "width" "300px"
    , Html.Attributes.style "height" "80px"
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



-- ---------------------------------------------------------------------------------------------------------------------
-- Internal functions
-- ---------------------------------------------------------------------------------------------------------------------
