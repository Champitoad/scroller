module Model.App exposing (..)

import Browser.Navigation
import Html5.DragDrop as DnD
import Model.Mascarpone exposing (..)
import Model.Program as Program exposing (Program, Route, Sandboxes, manualExamples)
import Model.Scroll exposing (..)
import Url



-- Drag-and-Drop


type DragSource
    = DragNode Id


type DropDestination
    = DropNode Id
    | DropLocation Location


type alias DragId =
    { route : Route, source : DragSource }


type alias DropId =
    Maybe { route : Route, destination : DropDestination }


type alias DnD =
    DnD.Model DragId DropId


type alias DnDMsg =
    DnD.Msg DragId DropId



-- Full state of the application


type alias Model =
    { program : Program
    , history : History
    , manualExamples : Sandboxes
    , dragDrop : DnD
    , url : Url.Url
    , key : Browser.Navigation.Key
    }


init : Url.Url -> Browser.Navigation.Key -> Model
init url key =
    { program = Program.fromNet mascarponeCreamRecipe
    , history = History { prev = Nothing, next = Nothing }
    , manualExamples = manualExamples
    , dragDrop = DnD.init
    , url = url
    , key = key
    }


getProgram : Route -> Model -> Program
getProgram route model =
    case route of
        Program.Playground ->
            model.program

        Program.Manual sandboxID ->
            (Program.getSandbox sandboxID model.manualExamples).currentProgram


setProgram : Route -> Program -> Model -> Model
setProgram route program model =
    case route of
        Program.Playground ->
            { model | program = program }

        Program.Manual sandboxID ->
            { model | manualExamples = Program.updateSandbox sandboxID program model.manualExamples }


setProgramWithHistory : Route -> Program -> Model -> Model
setProgramWithHistory route program model =
    case route of
        Program.Playground ->
            { model
                | program = program
                , history = History { prev = Just model, next = Nothing }
            }

        Program.Manual sandboxID ->
            { model | manualExamples = Program.updateSandbox sandboxID program model.manualExamples }



-- History of the full state mutually defined


type History
    = History
        { prev : Maybe Model
        , next : Maybe Model
        }


getHistory : Model -> { prev : Maybe Model, next : Maybe Model }
getHistory model =
    let
        (History history) =
            model.history
    in
    history


setHistory : { prev : Maybe Model, next : Maybe Model } -> Model -> Model
setHistory history model =
    { model | history = History history }


undo : Model -> Model
undo model =
    case (getHistory model).prev of
        Just prevModel ->
            let
                prevHistory =
                    getHistory prevModel
            in
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
