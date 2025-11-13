module Model.App exposing (..)

import Model.Scroll exposing (..)
import Model.Mascarpone exposing (..)
import Model.Goal as Goal exposing (Goal, Location, Sandboxes, manualExamples)

import Url
import Browser.Navigation
import Html5.DragDrop as DnD


-- Drag-and-Drop


type alias JudgmentDragId
  = { location : Location, source : Zipper, content : Computation }

type alias JudgmentDropId
  = Maybe { location : Location, target : Zipper, content : Net }

type alias JudgmentDnD
  = DnD.Model JudgmentDragId JudgmentDropId

type alias JudgmentDnDMsg
  = DnD.Msg JudgmentDragId JudgmentDropId


-- Full state of the application


type alias Model
  = { goal : Goal
    , history : History
    , manualExamples : Sandboxes
    , dragDrop : JudgmentDnD
    , url : Url.Url
    , key : Browser.Navigation.Key }


init : Url.Url -> Browser.Navigation.Key -> Model
init url key =
  { goal = Goal.fromNet [entails [crack, whisk, beat, stir, fold] [entails [f egg, f sugar, f mascarpone] [f mascarponeCream]]]
  , history = History { prev = Nothing, next = Nothing }
  , manualExamples = manualExamples
  , dragDrop = DnD.init
  , url = url
  , key = key
  }


getGoal : Location -> Model -> Goal
getGoal location model =
  case location of
    Goal.App ->
      model.goal
    Goal.Manual sandboxID ->
      (Goal.getSandbox sandboxID model.manualExamples).currentGoal


setGoal : Location -> Goal -> Model -> Model
setGoal location goal model =
  case location of
    Goal.App ->
      { model | goal = goal }
    Goal.Manual sandboxID ->
      { model | manualExamples = Goal.updateSandbox sandboxID goal model.manualExamples }


-- History of the full state mutually defined


type History
  = History { prev : Maybe Model
            , next : Maybe Model }


getHistory : Model -> { prev : Maybe Model, next : Maybe Model }
getHistory model =
  let (History history) = model.history in
  history


setHistory : { prev : Maybe Model, next : Maybe Model } -> Model -> Model
setHistory history model =
  { model | history = History history }


undo : Model -> Model
undo model =
  case (getHistory model).prev of
    Just prevModel ->
      let prevHistory = getHistory prevModel in
      setHistory { prevHistory | next = Just model } prevModel
    Nothing ->
      model


redo : Model -> Model
redo model =
  case (getHistory model).next of
    Just nextModel ->
      nextModel
    Nothing ->
      model