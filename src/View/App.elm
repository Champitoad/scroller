module View.App exposing (..)

import Browser exposing (Document)
import Element exposing (..)
import Html exposing (div)
import Html.Attributes exposing (id, style, tabindex)
import Html.Events exposing (on, preventDefaultOn)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key
import Model.App exposing (Model)
import Update.App exposing (..)
import Utils.Color
import View.Events
import View.Manual as Manual
import View.Route as Route
import View.Session exposing (..)
import View.Shelf exposing (..)
import View.Style exposing (..)
import View.Toolbar exposing (..)


keyboardListener : Html.Attribute Msg
keyboardListener =
    let
        handleAndPrevent : KeyboardEvent -> ( Msg, Bool )
        handleAndPrevent event =
            let
                prevent =
                    case ( event.keyCode, event.ctrlKey ) of
                        ( Keyboard.Key.Tab, _ ) ->
                            True

                        ( Keyboard.Key.R, True ) ->
                            True

                        _ ->
                            False
            in
            ( HandleKeyboardEvent event, prevent )
    in
    preventDefaultOn "keydown" <|
        Json.Decode.map handleAndPrevent decodeKeyboardEvent


keyUpListener : Html.Attribute Msg
keyUpListener =
    on "keyup" (Json.Decode.map HandleKeyUpEvent decodeKeyboardEvent)


view : Model -> Document Msg
view model =
    case Route.fromUrl model.url of
        Route.Playground ->
            let
                -- shelf =
                --     viewShelf model.playground
                session =
                    el
                        [ width fill
                        , sessionHeightAttr
                        ]
                        (viewSession model.dragDrop model.playground)

                toolbar =
                    el
                        [ width fill
                        , styleAttr "position" "fixed"
                        , styleAttr "bottom" "0"
                        ]
                        (viewToolbar model)

                app =
                    column [ width fill, height fill ] [ session, toolbar ]
                        |> layout []
            in
            { title = "Scroller Editor"
            , body =
                [ div
                    [ keyboardListener
                    , keyUpListener
                    , View.Events.trackDragModifiers
                    , id "app-container"
                    , tabindex 0
                    , style "outline" "none"
                    ]
                    [ app ]
                ]
            }

        Route.Manual ->
            { title = "Manual · Scroller"
            , body = [ layout [] (Manual.page model) ]
            }

        Route.NotFound url ->
            { title = "Error 404"
            , body =
                [ layout []
                    (column
                        (spacing 30 :: fillXY)
                        [ centered (el [ fontSize 50 ] (text "Page not found!"))
                        , centered
                            (el
                                [ fontSize 25
                                , rgb 0.6 0.6 0.6 |> Utils.Color.elementAttr
                                ]
                                (text url)
                            )
                        ]
                    )
                ]
            }
