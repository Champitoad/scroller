port module Update.App exposing (..)

import Model.Formula exposing (..)
import Model.Scroll exposing (..)
import Model.Goal exposing (..)
import Model.App exposing (..)

import Json.Decode exposing (Value)      

import Html5.DragDrop as DnD

import Keyboard.Event exposing (KeyboardEvent)

import Url
import Browser
import Browser.Navigation
import View.Route as Route


port dragstart : Value -> Cmd msg


type Msg
  = Apply Location Action
  | ExecAll
  | Step
  | ChangeActionMode ActionMode
  | ChangeExecMode ExecMode
  | ToggleRecording Bool
  | Undo
  | Redo
  | Auto
  | UpdateNewAtomName String
  | DragDropMsg ValDnDMsg
  | ResetSandbox SandboxID
  | HandleKeyboardEvent KeyboardEvent
  | ConsoleLog String String
  | DoNothing
  | UrlChanged Url.Url
  | LinkClicked Browser.UrlRequest


handleDragDropMsg : ValDnDMsg -> Model -> (Model, Cmd Msg)
handleDragDropMsg dndMsg model =
  let
    dragStart = 
      DnD.getDragstartEvent dndMsg

    cmd =
      dragStart
      |> Maybe.map (.event >> dragstart)
      |> Maybe.withDefault Cmd.none

    ( newDragDrop, result ) =
      DnD.update dndMsg model.dragDrop

    model_ =
      case dragStart of
        Just { dragId } ->
          let
            goal = getGoal dragId.location model
            
            newMode =
              case goal.actionMode of
                ProofMode Interacting ->
                  ProofMode Justifying
                EditMode modeData ->
                  EditMode { modeData | interaction = Reordering }
                _ ->
                  goal.actionMode
            
            newGoal =
              { goal | actionMode = newMode }
            
            newModel =
              { model | dragDrop = newDragDrop }
          in
          setGoal dragId.location newGoal newModel

        Nothing ->
          case result of
            Just (drag, drop, _) ->
              let
                goal = getGoal drag.location model
                
                defaultMode =
                  case goal.actionMode of
                    ProofMode _ ->
                      ProofMode Interacting

                    EditMode modeData ->
                      EditMode { modeData | interaction = Operating }

                    _ ->
                      goal.actionMode
              in
              case drop of
                -- Dropping on target
                Just dest ->
                  let
                    newGoal =
                      case goal.actionMode of
                        ProofMode Justifying ->
                          let
                            iterateVal =
                              IterateVal { srcCtx = drag.source, srcVal = drag.content
                                         , tgtCtx = dest.target, tgtName = Nothing }
                          in
                          apply iterateVal goal
                        
                        EditMode { interaction } ->
                          case interaction of
                            Reordering ->
                              let newFocus = fillZipper dest.content dest.target.zipper in
                              { goal | focus = newFocus }
                            _ ->
                              goal
                        
                        _ ->
                          goal

                    newModel =
                      setGoal goal.location { newGoal | actionMode = defaultMode } model
                  in
                  { newModel | dragDrop = newDragDrop }

                -- Dropping on non-target
                Nothing ->
                  let newModel = setGoal goal.location { goal | actionMode = defaultMode } model in
                  { newModel | dragDrop = newDragDrop }
        
            -- Dragging
            Nothing ->
              { model | dragDrop = newDragDrop }
  in
  (model_, cmd)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Apply location action ->
      let newGoal = apply action (getGoal location model) in
      (setGoalWithHistory location newGoal model, Cmd.none)
    
    ExecAll ->
      ({ model | goal = execAll model.goal }, Cmd.none)
     
    Step ->
      Debug.todo "Action stepping not implemented yet"

    ChangeActionMode mode ->
      ({ model | goal = changeActionMode mode model.goal }, Cmd.none)
    
    ChangeExecMode mode ->
      ({ model | goal = changeExecMode mode model.goal }, Cmd.none)
    
    ToggleRecording recording ->
      ({ model | goal = toggleRecording recording model.goal }, Cmd.none)

    Undo ->
      (undo model, Cmd.none)

    Redo ->
      (redo model, Cmd.none)

    Auto ->
      Debug.todo "Auto not implemented yet"
    
    UpdateNewAtomName name ->
      ({ model | goal = updateNewAtomName name model.goal }, Cmd.none)

    DragDropMsg dndMsg ->
      handleDragDropMsg dndMsg model
    
    ResetSandbox id ->
      ( { model | manualExamples = resetSandbox id model.manualExamples }
      , Cmd.none )
    
    HandleKeyboardEvent { ctrlKey, key } ->
      let
        newModel =
          case (ctrlKey, key) of
            (True, Just "z") -> update Undo model |> Tuple.first
            (True, Just "y") -> update Redo model |> Tuple.first
            _ -> model
      in
      (newModel, Cmd.none)
    
    ConsoleLog tag message ->
      let _ = Debug.log tag message in
      (model, Cmd.none)
    
    DoNothing ->
      (model, Cmd.none)
    
    UrlChanged url ->
      let
        newModel =
          case Route.fromUrl url of
            Route.Manual ->
              { model | manualExamples = resetAllSandboxes model.manualExamples }
            _ ->
              model
      in
      ({ newModel | url = url }, Cmd.none)
    
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          let 
            newPath =
              case url.host of
                "www.lix.polytechnique.fr" ->
                  "/Labo/Pablo.DONATO/flowerprover" ++ url.path
                "pablogician.refl.fr" ->
                  "/flowerprover" ++ url.path
                _ ->
                  url.path
            
            newUrl =
              { url | path = newPath }  
          in
          ( model, Browser.Navigation.pushUrl model.key (Url.toString newUrl) )

        Browser.External href ->
          ( model, Browser.Navigation.load href )
