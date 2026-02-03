module View.Events exposing (..)

import Color
import Element exposing (..)
import Html
import Html.Attributes exposing (..)
import Html.Events
import Html5.DragDrop as DnD
import Json.Decode
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


dragAction : Color.Color -> DnD -> Route -> Id -> List (Attribute Msg)
dragAction color dnd route id =
    let
        draggableStyle =
            View.Style.draggable color

        style =
            case DnD.getDragId dnd of
                Just { source } ->
                    let
                        (DragNode sourceId) =
                            source
                    in
                    if sourceId == id then
                        draggableStyle.active

                    else
                        draggableStyle.inactive

                Nothing ->
                    draggableStyle.inactive
    in
    style
        ++ (List.map htmlAttribute <|
                DnD.draggable DragDropMsg
                    { route = route, source = DragNode id }
           )


trackDragModifiers : Html.Attribute Msg
trackDragModifiers =
    let
        decoder =
            Json.Decode.field "altKey" Json.Decode.bool
                |> Json.Decode.map (\alt -> SetDragModifiers { alt = alt })
    in
    Html.Events.on "dragover" decoder
