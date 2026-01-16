module View.App exposing (..)

import Browser exposing (Document)
import Element exposing (..)
import Element.Font as Font
import Html exposing (div)
import Html.Attributes exposing (style, tabindex)
import Html.Events exposing (on)
import Json.Decode
import Keyboard.Event exposing (decodeKeyboardEvent)
import Model.App exposing (Model)
import Update.App exposing (..)
import View.Manual as Manual
import View.Program exposing (..)
import View.Route as Route
import View.Shelf exposing (..)
import View.Style exposing (centered, fillXY, styleAttr)
import View.Toolbar exposing (..)


keyboardListener : Html.Attribute Msg
keyboardListener =
    on "keydown" <|
        Json.Decode.map HandleKeyboardEvent decodeKeyboardEvent


view : Model -> Document Msg
view model =
    case Route.fromUrl model.url of
        Route.Playground ->
            let
                shelf =
                    viewShelf model.goal

                goal =
                    el
                        [ width fill
                        , goalHeightAttr
                        ]
                        (viewProgram model.dragDrop model.goal)

                toolbar =
                    el
                        [ width fill
                        , styleAttr "position" "fixed"
                        , styleAttr "bottom" "0"
                        ]
                        (viewToolbar model)

                app =
                    column [ width fill, height fill ] [ goal, toolbar ]
                        |> layout []
            in
            { title = "Flower Prover"
            , body =
                [ div
                    [ keyboardListener
                    , tabindex 0
                    ]
                    [ app ]
                ]
            }

        Route.Manual ->
            { title = "Manual · Flower Prover"
            , body = [ layout [] (Manual.page model) ]
            }

        Route.NotFound url ->
            { title = "Error 404"
            , body =
                [ layout []
                    (column
                        (spacing 30 :: fillXY)
                        [ centered (el [ Font.size 50 ] (text "Page not found!"))
                        , centered (el [ Font.size 25, Font.color (rgb 0.6 0.6 0.6) ] (text url))
                        ]
                    )
                ]
            }
