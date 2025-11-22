module View.Toolbar exposing (..)

import View.Style exposing (..)
import View.Widgets exposing (..)

import Model.Formula as Formula
import Model.Goal exposing (..)
import Model.App exposing (..)

import Update.App exposing (..)

import Utils.Color
import Utils.Maybe exposing (..)
import Utils.Events exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input

import Html.Attributes

import FeatherIcons as Icons


type ModeSelectorPosition
  = Start | Middle | End


viewAutoButton : ActionMode -> Element Msg
viewAutoButton mode =
  let
    enabled =
      case mode of
        ProofMode _ -> True
        _ -> False
  in
  defaultButton
    { action = Msg Auto
    , title = "Auto"
    , icon = Icons.zap
    , enabled = enabled }


viewHelpButton : Element Msg
viewHelpButton =
  defaultButton
    { action = Link "/manual"
    , title = "Help"
    , icon = Icons.helpCircle
    , enabled = True }


viewNewAtomNameTextEdit : String -> Element Msg
viewNewAtomNameTextEdit newAtomName =
  Input.text
    [ width (105 |> px)
    , Border.rounded scrollBorderRound
    , onClick DoNothing ]
    { onChange = UpdateNewAtomName
    , text = newAtomName
    , placeholder = Just (Input.placeholder [] (text "new atom name"))
    , label = Input.labelHidden "New atom name" }


viewModeSelector : ActionMode -> Element Msg
viewModeSelector currentMode =
  let
    item mode position =
      let
        isSelected =
          case (mode, currentMode) of          
            (ProofMode _, ProofMode _) -> True
            (EditMode _, EditMode _) -> True
            _ -> mode == currentMode

        (bgColor, fgColor) =
          if isSelected
          then (rgb255 58 134 255, rgb 1 1 1)
          else (rgb 1 1 1, rgb 0 0 0)

        (iconEl, titleText) =
          let
            (title, icon) =
              case mode of
                ProofMode _ -> ("Prove", Icons.checkSquare)
                EditMode _ -> ("Edit", Icons.edit2)
                NavigationMode -> ("Navigate", Icons.navigation)
            elem =
              el
                [ centerX, centerY ]
                ( icon
                  |> Icons.withSize 30
                  |> Icons.toHtml [ fgColor 
                                    |> Utils.Color.fromElement
                                    |> Utils.Color.toHtmlAttr ]
                  |> html )
          in
          (elem, title)
        
        borderRound =
          case position of
            Start ->
              { topLeft = buttonBorderRadius, bottomLeft = buttonBorderRadius
              , topRight = 0, bottomRight = 0 }
            Middle ->
              { topLeft = 0, bottomLeft = 0
              , topRight = 0, bottomRight = 0 }
            End ->
              { topLeft = 0, bottomLeft = 0
              , topRight = buttonBorderRadius, bottomRight = buttonBorderRadius }

        changeAction =
          [ Events.onClick (ChangeActionMode mode)
          , pointer ]
      in
      el
        ( [ width (60 |> px)
          , height (defaultButtonSize |> px)
          , Background.color bgColor
          , Border.roundEach borderRound
          , htmlAttribute <| Html.Attributes.title titleText ]
         ++ changeAction )
        iconEl
    
    borderColor = rgb 0.6 0.6 0.6
  in
  row
    [ width shrink
    , height shrink
    , spacing 1
    , Border.width 0
    , Border.rounded buttonBorderRadius
    , Border.color borderColor
    , Background.color borderColor ]
    [ item (ProofMode Argumenting) Start
    , item (EditMode { interaction = Operating
                     , surgery = initialSurgery
                     , newAtomName = "a" }) Middle
    , item NavigationMode End ]


viewUndoRedo : History -> Element Msg
viewUndoRedo (History history) =
  let
    (undoEnabled, redoEnabled) =
      Tuple.mapBoth isSomething isSomething (history.prev, history.next)
  in
  row []
    [ defaultButton
        { action = Msg Undo
        , title = "Undo"
        , icon = Icons.arrowLeft
        , enabled = undoEnabled }
    , defaultButton
        { action = Msg Redo
        , title = "Redo"
        , icon = Icons.arrowRight
        , enabled = redoEnabled } ]


viewToolbar : Model -> Element Msg
viewToolbar model =
  let
    -- autoButton = viewAutoButton model.goal.actionMode
    newAtomNameTextEdit =
      case model.goal.actionMode of
        EditMode { newAtomName } ->
          [viewNewAtomNameTextEdit newAtomName]
        _ ->
          []
      
    modeSelector = viewModeSelector model.goal.actionMode
    undoRedo = viewUndoRedo model.history
  in
  row
    [ width fill
    , height shrink
    , padding 15
    , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
    , Border.color (rgb 0.6 0.6 0.6)
    -- , Border.shadow
    --     { offset = (0, -3)
    --     , size = 0.1
    --     , blur = 5
    --     , color = rgb 0.5 0.5 0.5 }
    , Background.gradient
        { angle = 0
        , steps = [ rgb 0.8 0.8 0.8, rgb 0.9 0.9 0.9 ] } ]
    [ row
        [ width fill
        , spacing 5 ]
        ( el
            [ alignLeft ]
            ( viewHelpButton ) ::
          newAtomNameTextEdit )
        -- , el
        --     [ alignLeft ]
        --     ( autoButton ) ]
    , modeSelector
    , el
        [ width fill ]
        ( el
            [ alignRight ]
            ( undoRedo ) ) ]