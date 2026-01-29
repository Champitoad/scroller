module View.Style exposing (..)

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes exposing (style)
import Model.App exposing (..)
import Model.Scroll exposing (..)
import Utils.Color


styleAttr : String -> String -> Attribute msg
styleAttr attr val =
    htmlAttribute <| style attr val



-- Font styling with fallback for sizes that don't work with Font.size


fontSize : Int -> Attribute msg
fontSize size =
    -- Use styleAttr for all font sizes to ensure consistency
    -- and avoid the 33 threshold issue with Font.size
    styleAttr "font-size" (String.fromInt size ++ "px")



-- Interaction


nonSelectable : Attribute msg
nonSelectable =
    styleAttr "user-select" "none"



-- Text


bold : String -> Element msg
bold txt =
    el [ Font.bold ]
        (text txt)


italic : String -> Element msg
italic txt =
    el [ Font.italic ]
        (text txt)



-- Layout


fillXY : List (Attribute msg)
fillXY =
    [ width fill, height fill ]


centered : Element msg -> Element msg
centered elem =
    el [ centerX, centerY ] elem



-- Colors


transparent : Color
transparent =
    rgba 0 0 0 0


negativeColor : Color
negativeColor =
    rgb 0.18 0.2 0.25


positiveColor : Color
positiveColor =
    rgb 1 1 1


foregroundColor : Polarity -> Color
foregroundColor polarity =
    case polarity of
        Pos ->
            negativeColor

        Neg ->
            positiveColor


backgroundColor : Polarity -> Color
backgroundColor polarity =
    case polarity of
        Pos ->
            positiveColor

        Neg ->
            negativeColor


introColor : Color
introColor =
    rgb 0.227 0.525 1


elimColor : Color
elimColor =
    rgb 1 0 0


reorderColor : Color.Color
reorderColor =
    Color.rgb 0.7 0.7 0.7


useColor : Color.Color
useColor =
    Color.rgb 1 0.8 0



-- Scroll styling


scrollBorderWidth : Int
scrollBorderWidth =
    3


scrollBorderRound : Int
scrollBorderRound =
    10


type alias ZoneStyle msg =
    { borderWidth : Int
    , active : List (Attribute msg)
    , inactive : List (Attribute msg)
    }


actionable : Color.Color -> ZoneStyle msg
actionable color =
    let
        width =
            5

        border =
            [ styleAttr "border-width" (String.fromInt width ++ "px")
            , styleAttr "border-style" "dotted"
            , styleAttr "border-radius" (String.fromInt scrollBorderRound ++ "px")
            ]

        bgColor =
            Utils.Color.withAlpha 0.5 color |> Utils.Color.toElement
    in
    { borderWidth = width
    , active =
        pointer
            :: styleAttr "border-color" (Color.toCssString color)
            :: Background.color bgColor
            :: border
    , inactive =
        styleAttr "border-color" "transparent"
            :: border
    }


greenActionable : ZoneStyle msg
greenActionable =
    actionable (Color.rgb 0.3 0.9 0.3)


pinkActionable : ZoneStyle msg
pinkActionable =
    actionable (Color.rgb 1 0.4 0.8)


orangeActionable : ZoneStyle msg
orangeActionable =
    actionable (Color.rgb 1 0.6 0)


redActionable : ZoneStyle msg
redActionable =
    actionable (Color.rgb 1 0 0)


draggable : Color.Color -> ZoneStyle msg
draggable color =
    let
        width =
            3

        border =
            [ styleAttr "border-width" (String.fromInt width ++ "px")
            , styleAttr "border-style" "solid"
            , styleAttr "border-radius" (String.fromInt scrollBorderRound ++ "px")
            ]

        borderColor =
            Color.toCssString color
    in
    { borderWidth = width
    , active = styleAttr "border-color" borderColor :: border
    , inactive = styleAttr "border-color" "transparent" :: border
    }


droppable : Color.Color -> ZoneStyle msg
droppable color =
    let
        width =
            3

        border =
            [ styleAttr "border-width" (String.fromInt width ++ "px")
            , styleAttr "border-style" "dashed"
            , styleAttr "border-radius" (String.fromInt scrollBorderRound ++ "px")
            , styleAttr "border-color" (Color.toCssString color)
            ]

        bgColor =
            Utils.Color.withAlpha 0.5 color |> Utils.Color.toElement
    in
    { borderWidth = width
    , active = Background.color bgColor :: border
    , inactive = border
    }


grownBorder : ZoneStyle msg
grownBorder =
    let
        width =
            3

        border =
            [ styleAttr "border-radius" (String.fromInt scrollBorderRound ++ "px") ]
    in
    { borderWidth = width
    , active =
        styleAttr "border-color" (introColor |> Utils.Color.fromElement |> Color.toCssString)
            :: styleAttr "border-style" "solid"
            :: styleAttr "border-width" (String.fromInt width ++ "px")
            :: border
    , inactive =
        border
    }
