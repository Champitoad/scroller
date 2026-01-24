module View.Shelf exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Model.Session exposing (..)
import Update.App exposing (..)
import View.Session exposing (..)


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
        , Border.color (rgb 0.6 0.6 0.6)
        , Border.widthEach { bottom = 3, left = 0, right = 0, top = 0 }
        , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 30, bottomRight = 30 }
        ]
        []
