module View.Toolbar exposing (..)

import View.Style exposing (..)
import View.Widgets exposing (..)

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
import Queue
import View.Widgets as Widgets


type ModeSelectorPosition
  = Start | Middle | End


modeSelectorColor : Color
modeSelectorColor =
  rgb 0.5 0.5 0.5

modeSelectorBorderRound : ModeSelectorPosition -> { topLeft : Int, topRight : Int, bottomLeft : Int, bottomRight : Int }
modeSelectorBorderRound position =
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
    [ Border.rounded scrollBorderRound
    , onClick DoNothing ]
    { onChange = UpdateNewAtomName
    , text = newAtomName
    , placeholder = Just (Input.placeholder [] (text "New atom name"))
    , label = Input.labelHidden "New atom name" }


viewActionModeSelector : ActionMode -> Element Msg
viewActionModeSelector currentMode =
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
          then (modeSelectorColor, rgb 1 1 1)
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
          modeSelectorBorderRound position

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
    [ item (ProofMode Justifying) Start
    , item (EditMode { interaction = Operating
                     , surgery = initialSurgery
                     , newAtomName = "" }) Middle
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


viewExecModeSelector : ExecMode -> Element Msg
viewExecModeSelector currentMode =
  let
    item mode position =
      let
        isSelected =
          mode == currentMode

        (bgColor, fgColor) =
          if isSelected
          then (modeSelectorColor, rgb 1 1 1)
          else (rgb 1 1 1, rgb 0 0 0)

        (iconEl, titleText) =
          let
            (title, icon) =
              case mode of
                Forward -> ("Forward", Icons.chevronsRight)
                Backward -> ("Backward", Icons.chevronsLeft)
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
          modeSelectorBorderRound position

        changeAction =
          [ Events.onClick (ChangeExecMode mode)
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
    [ item Backward Start
    , item Forward End ]

         
viewRecordToggle : Bool -> Element Msg
viewRecordToggle recording =
  toggle
    { color = modeSelectorColor
    , iconOn = Icons.camera
    , iconOff = Icons.cameraOff
    , title = "Record"
    , onChange = ToggleRecording }
    recording


viewExecButtons : Goal -> Element Msg
viewExecButtons { actionsQueue } =
  let
    stepEnabled =
      not (Queue.isEmpty actionsQueue)

    stepButton =
      defaultButton
        { action = Msg Step
        , title = "Step"
        , icon = Icons.play
        , enabled = stepEnabled }
    
    runButton =
      defaultButton
        { action = Msg ExecAll
        , title = "Run"
        , icon = Icons.skipForward
        , enabled = True }
  in
  row [] [stepButton, runButton]


viewToolbar : Model -> Element Msg
viewToolbar model =
  let
    -- autoButton = viewAutoButton model.goal.actionMode
    helpButton = viewHelpButton
    actionModeSelector = viewActionModeSelector model.goal.actionMode
    undoRedo = viewUndoRedo model.history
    
    actionToolZone =
      row
        [ paddingXY 15 0
        , spacing 10
        , centerX
        ]
        ( case model.goal.actionMode of
            EditMode { newAtomName } ->
              [ el [ width (fill |> maximum 200)
                   , centerX
                   ]
                   ( viewNewAtomNameTextEdit newAtomName )
              ]
            _ ->
              []
        )

    execZone =
      row  
        [ paddingXY 15 0
        , spacing 20
        , centerX
        ]
        [ viewExecModeSelector model.goal.execMode
        , viewRecordToggle model.goal.recording
        , viewExecButtons model.goal
        ]
  in
  row
    [ width fill
    , height shrink
    , padding 15
    , spacing 100
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
    [ el [ alignLeft ] helpButton
    , el [ width fill ] (actionToolZone)
    , el [ centerX ] actionModeSelector
    , el [ width fill ] (execZone)
    , el [ alignRight ] undoRedo ]