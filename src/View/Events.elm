module View.Events exposing (..)

import Color
import Element exposing (..)
import Html.Attributes exposing (..)
import Html5.DragDrop as DnD
import Model.App exposing (..)
import Model.Scroll exposing (..)
import Model.Session exposing (..)
import Update.App exposing (..)
import Utils.Events exposing (..)
import View.Style


stopPropagation : List (Attribute Msg)
stopPropagation =
    [ onDragOver DoNothing
    , onMouseMove DoNothing
    ]


dragAction : Color.Color -> DnD -> Route -> Context -> Val -> List (Attribute Msg)
dragAction color dnd location ctx val =
    if List.length ctx.zipper <= 1 then
        []

    else
        let
            draggableStyle =
                View.Style.draggable color

            style =
                case DnD.getDragId dnd of
                    Just { source, content } ->
                        if source.zipper == ctx.zipper && content == val then
                            draggableStyle.active

                        else
                            draggableStyle.inactive

                    Nothing ->
                        draggableStyle.inactive
        in
        style
            ++ (List.map htmlAttribute <|
                    DnD.draggable DragDropMsg
                        { location = location, source = ctx, content = val }
               )
