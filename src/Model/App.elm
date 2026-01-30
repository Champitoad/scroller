module Model.App exposing (..)

import Browser.Navigation
import Html5.DragDrop as DnD
import Model.Mascarpone exposing (..)
import Model.Scroll as Scroll exposing (..)
import Model.Session as Session exposing (..)
import Url



-- Drag-and-Drop


type DragSource
    = DragNode Id


type DropDestination
    = DropNode Id
    | DropContext Context
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
    { session : Session
    , history : History
    , manualExamples : Sandboxes
    , dragDrop : DnD
    , url : Url.Url
    , key : Browser.Navigation.Key
    }


init : Url.Url -> Browser.Navigation.Key -> Model
init url key =
    { session = Session.fromNet (netOfStruct [ modusPonensCurryfied ])
    , history = History { prev = Nothing, next = Nothing }
    , manualExamples = manualExamples
    , dragDrop = DnD.init
    , url = url
    , key = key
    }


getSession : Route -> Model -> Session
getSession route model =
    case route of
        Session.Playground ->
            model.session

        Session.Manual sandboxID ->
            (Session.getSandbox sandboxID model.manualExamples).currentSession


setSession : Route -> Session -> Model -> Model
setSession route session model =
    case route of
        Session.Playground ->
            { model | session = session }

        Session.Manual sandboxID ->
            { model | manualExamples = Session.updateSandbox sandboxID session model.manualExamples }


setSessionWithHistory : Route -> Session -> Model -> Model
setSessionWithHistory route session model =
    case route of
        Session.Playground ->
            { model
                | session = session
                , history = History { prev = Just model, next = Nothing }
            }

        Session.Manual sandboxID ->
            { model | manualExamples = Session.updateSandbox sandboxID session model.manualExamples }



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
