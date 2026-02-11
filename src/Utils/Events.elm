module Utils.Events exposing (..)

import Element exposing (..)
import Html.Events
import Html.Styled
import Html.Styled.Events
import Json.Decode as Json


onClick : msg -> Attribute msg
onClick =
    alwaysStopPropagationOn "click"


onClickStyled : msg -> Html.Styled.Attribute msg
onClickStyled =
    alwaysStopPropagationOnStyled "click"


onMouseDown : msg -> Attribute msg
onMouseDown =
    alwaysStopPropagationOn "mousedown"


onMouseUp : msg -> Attribute msg
onMouseUp =
    alwaysStopPropagationOn "mouseup"


onTouchStart : msg -> Html.Styled.Attribute msg
onTouchStart =
    alwaysStopPropagationOnStyled "touchstart"


onTouchEnd : msg -> Html.Styled.Attribute msg
onTouchEnd =
    alwaysStopPropagationOnStyled "touchend"


onTouchCancel : msg -> Html.Styled.Attribute msg
onTouchCancel =
    alwaysStopPropagationOnStyled "touchcancel"


onMouseMove : msg -> Attribute msg
onMouseMove =
    alwaysStopPropagationOn "mousemove"


onDragOver : msg -> Attribute msg
onDragOver =
    alwaysStopPropagationOn "dragover"


alwaysStopPropagationOn : String -> msg -> Attribute msg
alwaysStopPropagationOn event msg =
    let
        always m =
            ( m, True )
    in
    Html.Events.stopPropagationOn event (Json.map always (Json.succeed msg))
        |> htmlAttribute


alwaysStopPropagationOnStyled : String -> msg -> Html.Styled.Attribute msg
alwaysStopPropagationOnStyled event msg =
    let
        always m =
            ( m, True )
    in
    Html.Styled.Events.stopPropagationOn event (Json.map always (Json.succeed msg))
