module View.Style exposing (..)

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Attributes exposing (style)
import Model.App exposing (..)
import Model.Scroll exposing (..)
import Model.Session exposing (..)
import Utils.Color
import Utils.Events


styleAttr : String -> String -> Attribute msg
styleAttr attr val =
    htmlAttribute <| style attr val



-- Fonts


nameFontFamily : Attribute msg
nameFontFamily =
    styleAttr "font-family" "Fira Code"



{- Fallback for sizes that don't work with Font.size -}


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


createColor : Color
createColor =
    rgb 0.227 0.525 1


destroyColor : Color
destroyColor =
    rgb 1 0 0


expandColor : Color
expandColor =
    rgb 0.3 0.8 0


collapseColor : Color
collapseColor =
    rgb 1 0.6 0


reorderColor : Color.Color
reorderColor =
    Color.rgb 0.7 0.7 0.7


useColor : Color.Color
useColor =
    Color.rgb 1 0.8 0



-- Scroll styling


sepBorderWidth : Int
sepBorderWidth =
    3


sepBorderRound : Int
sepBorderRound =
    25


sepBorderRadius : Attribute msg
sepBorderRadius =
    let
        borderRadiusCssStr =
            String.fromInt sepBorderRound ++ "px"
    in
    styleAttr "border-radius"
        ("0px 0px "
            ++ borderRadiusCssStr
            ++ " "
            ++ borderRadiusCssStr
        )


shapeBorderRadius : Maybe Shape -> Attribute msg
shapeBorderRadius shape =
    case shape of
        Just (Sep _ _) ->
            sepBorderRadius

        Nothing ->
            styleAttr "border-radius" "20px"

        _ ->
            styleAttr "border-radius" "none"


type alias ZoneStyle msg =
    { borderWidth : Int
    , active : List (Attribute msg)
    , inactive : List (Attribute msg)
    }


actionable : msg -> Maybe Shape -> Color -> ZoneStyle msg
actionable msg shape color =
    let
        borderWidth =
            5

        border =
            [ styleAttr "border-width" (String.fromInt borderWidth ++ "px")
            , styleAttr "border-style" "dotted"
            , styleAttr "border-color" (color |> Utils.Color.fromElement |> Color.toCssString)
            , shapeBorderRadius shape
            ]

        bgColor =
            color |> Utils.Color.fromElement |> Utils.Color.withAlpha 0.0

        hoverColor =
            color |> Utils.Color.fromElement |> Utils.Color.withAlpha 0.0

        activeColor =
            color |> Utils.Color.fromElement |> Utils.Color.withAlpha 0.5
    in
    { borderWidth = borderWidth
    , active =
        [ inFront
            (Input.button
                [ width fill
                , height fill
                , shapeBorderRadius shape
                , pointer
                , Background.color (bgColor |> Utils.Color.toElement)
                , mouseDown [ Background.color (activeColor |> Utils.Color.toElement) ]
                , mouseOver [ Background.color (hoverColor |> Utils.Color.toElement) ]
                , Utils.Events.onClick msg
                ]
                { onPress = Just msg
                , label = none
                }
            )
        ]
    , inactive = border
    }


green : Color
green =
    rgb 0.3 0.9 0.3


pink : Color
pink =
    rgb 1 0.4 0.8


draggable : Color.Color -> ZoneStyle msg
draggable color =
    let
        width =
            3

        border =
            [ styleAttr "border-width" (String.fromInt width ++ "px")
            , styleAttr "border-style" "solid"
            ]

        borderColor =
            Color.toCssString color
    in
    { borderWidth = width
    , active = styleAttr "border-color" borderColor :: border
    , inactive = []
    }


droppableBorderWidth : Int
droppableBorderWidth =
    3


droppableBorder : Color.Color -> List (Attribute msg)
droppableBorder color =
    [ styleAttr "border-width" (String.fromInt droppableBorderWidth ++ "px")
    , styleAttr "border-style" "dashed"
    , styleAttr "border-radius" (String.fromInt sepBorderRound ++ "px")
    , styleAttr "border-color" (Color.toCssString color)
    ]


droppableBackground : Color.Color -> Attribute msg
droppableBackground color =
    color |> Utils.Color.withAlpha 0.5 |> Utils.Color.toElement |> Background.color


droppableArea : Color.Color -> ZoneStyle msg
droppableArea color =
    { borderWidth = droppableBorderWidth
    , active = droppableBackground color :: droppableBorder color
    , inactive = droppableBorder color
    }


droppableNode : Color.Color -> ZoneStyle msg
droppableNode color =
    { borderWidth = droppableBorderWidth
    , active =
        inFront
            (el
                [ width fill
                , height fill
                , droppableBackground color
                , styleAttr "pointer-events" "none"
                ]
                none
            )
            :: droppableBorder color
    , inactive = droppableBorder color
    }


insertedBorder : ZoneStyle msg
insertedBorder =
    let
        width =
            3

        border =
            []
    in
    { borderWidth = width
    , active =
        styleAttr "border-color" (createColor |> Utils.Color.fromElement |> Color.toCssString)
            :: styleAttr "border-style" "solid"
            :: styleAttr "border-width" (String.fromInt width ++ "px")
            :: border
    , inactive =
        border
    }
