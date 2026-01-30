module Utils.Color exposing (..)

import Color exposing (..)
import Css
import Element
import Html
import Html.Attributes exposing (style)


type alias Color =
    Color.Color



-- Color model conversions


toRgba255 : Color -> { red : Int, green : Int, blue : Int, alpha : Float }
toRgba255 color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    let
        int f =
            round (f * 255)
    in
    { red = int red
    , green = int green
    , blue = int blue
    , alpha = alpha
    }



-- Library conversions


fromElement : Element.Color -> Color
fromElement color =
    color |> Element.toRgb |> Color.fromRgba


toElement : Color -> Element.Color
toElement color =
    color |> Color.toRgba |> Element.fromRgb


toCss : Color -> Css.Color
toCss color =
    let
        { red, green, blue, alpha } =
            toRgba255 color
    in
    Css.rgba red green blue alpha



-- Utility functions


htmlAttr : Color -> Html.Attribute msg
htmlAttr color =
    style "color" (Color.toCssString color)


elementAttr : Element.Color -> Element.Attribute msg
elementAttr color =
    color |> fromElement |> htmlAttr |> Element.htmlAttribute


transparent : Color
transparent =
    Color.fromRgba { red = 1, green = 1, blue = 1, alpha = 0 }


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    let
        rgba =
            Color.toRgba color
    in
    Color.fromRgba { rgba | alpha = alpha }


changeLight : Float -> Color -> Color
changeLight factor color =
    let
        ({ lightness } as hsla) =
            Color.toHsla color
    in
    Color.fromHsla { hsla | lightness = factor * lightness }
