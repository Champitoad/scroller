module View.Shelf exposing (..)

import Element exposing (..)
import Element.Background as Background
import Model.Session exposing (..)
import Update.App exposing (..)
import View.Session exposing (..)
import View.Style exposing (styleAttr)


viewShelf : Session -> Element Msg
viewShelf session =
    row
        [ width fill
        , height (fillPortion 1)
        , padding 15
        , Background.gradient
            { angle = 0
            , steps = [ rgb 0.85 0.85 0.85, rgb 0.9 0.9 0.9 ]
            }
        , styleAttr "border-color" "rgb(153, 153, 153)"
        , styleAttr "border-width" "0px 0px 3px 0px"
        , styleAttr "border-style" "solid"
        , styleAttr "border-radius" "0px 0px 30px 30px"
        ]
        []
