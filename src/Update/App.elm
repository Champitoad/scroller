port module Update.App exposing (..)

import Browser
import Browser.Navigation
import Html5.DragDrop as DnD
import Json.Decode exposing (Value)
import Keyboard.Event exposing (KeyboardEvent)
import Model.App exposing (..)
import Model.Formula exposing (..)
import Model.Scroll exposing (..)
import Model.Session exposing (..)
import Url
import View.Route as Route


port dragstart : Value -> Cmd msg


type Msg
    = Apply Route Action
    | Transform Route (Session -> Session)
    | Exec Route Int
    | ExecAll
    | Step
    | ChangeActionMode ActionMode
    | ChangeExecMode ExecMode
    | ToggleRecording Bool
    | Undo
    | Redo
    | Auto
    | UpdateNewAtomName String
    | DragDropMsg DnDMsg
    | ResetSandbox SandboxID
    | HandleKeyboardEvent KeyboardEvent
    | ConsoleLog String String
    | DoNothing
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


handleDragDropMsg : DnDMsg -> Model -> ( Model, Cmd Msg )
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
                        session =
                            getSession dragId.route model

                        newMode =
                            case session.actionMode of
                                ProofMode Interacting ->
                                    ProofMode Justifying

                                EditMode modeData ->
                                    EditMode { modeData | interaction = Reordering }

                                _ ->
                                    session.actionMode

                        newSession =
                            { session | actionMode = newMode }

                        newModel =
                            { model | dragDrop = newDragDrop }
                    in
                    setSession dragId.route newSession newModel

                Nothing ->
                    case result of
                        Just ( drag, drop, _ ) ->
                            let
                                (DragNode source) =
                                    drag.source

                                session =
                                    getSession drag.route model

                                defaultMode =
                                    case session.actionMode of
                                        ProofMode _ ->
                                            ProofMode Interacting

                                        EditMode modeData ->
                                            EditMode { modeData | interaction = Operating }

                                        _ ->
                                            session.actionMode
                            in
                            case drop of
                                -- Dropping on valid destination
                                Just { destination } ->
                                    let
                                        newSession =
                                            case ( session.actionMode, destination ) of
                                                ( ProofMode Justifying, DropNode target ) ->
                                                    apply (Deiterate source target) session

                                                ( ProofMode Justifying, DropLocation location ) ->
                                                    apply (Iterate source location) session

                                                ( EditMode _, DropLocation location ) ->
                                                    apply (Reorder source location.pos) session

                                                _ ->
                                                    session

                                        newModel =
                                            setSession session.route { newSession | actionMode = defaultMode } model
                                    in
                                    { newModel | dragDrop = newDragDrop }

                                -- Dropping on invalid destination
                                Nothing ->
                                    let
                                        newModel =
                                            setSession session.route { session | actionMode = defaultMode } model
                                    in
                                    { newModel | dragDrop = newDragDrop }

                        -- Dragging
                        Nothing ->
                            { model | dragDrop = newDragDrop }
    in
    ( model_, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Apply route action ->
            let
                newSession =
                    apply action (getSession route model)
            in
            ( setSessionWithHistory route newSession model, Cmd.none )

        Transform route transform ->
            let
                newSession =
                    transform (getSession route model)
            in
            ( setSessionWithHistory route newSession model, Cmd.none )

        Exec route actionId ->
            let
                newSession =
                    execute actionId (getSession route model)
            in
            ( setSessionWithHistory route newSession model, Cmd.none )

        ExecAll ->
            ( { model | session = execAll model.session }, Cmd.none )

        Step ->
            Debug.todo "Action stepping not implemented yet"

        ChangeActionMode mode ->
            ( { model | session = changeActionMode mode model.session }, Cmd.none )

        ChangeExecMode mode ->
            ( { model | session = changeExecMode mode model.session }, Cmd.none )

        ToggleRecording recording ->
            ( { model | session = toggleRecording recording model.session }, Cmd.none )

        Undo ->
            ( undo model, Cmd.none )

        Redo ->
            ( redo model, Cmd.none )

        Auto ->
            Debug.todo "Auto not implemented yet"

        UpdateNewAtomName name ->
            ( { model | session = updateNewAtomName name model.session }, Cmd.none )

        DragDropMsg dndMsg ->
            handleDragDropMsg dndMsg model

        ResetSandbox id ->
            ( { model | manualExamples = resetSandbox id model.manualExamples }
            , Cmd.none
            )

        HandleKeyboardEvent { ctrlKey, key } ->
            let
                newModel =
                    case ( ctrlKey, key ) of
                        ( True, Just "z" ) ->
                            update Undo model |> Tuple.first

                        ( True, Just "y" ) ->
                            update Redo model |> Tuple.first

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        ConsoleLog tag message ->
            let
                _ =
                    Debug.log tag message
            in
            ( model, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )

        UrlChanged url ->
            let
                newModel =
                    case Route.fromUrl url of
                        Route.Manual ->
                            { model | manualExamples = resetAllSandboxes model.manualExamples }

                        _ ->
                            model
            in
            ( { newModel | url = url }, Cmd.none )

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
