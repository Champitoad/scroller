port module Update.App exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Html5.DragDrop as DnD
import Json.Decode exposing (Value)
import Keyboard.Event exposing (KeyboardEvent)
import Keyboard.Key
import Model.App exposing (..)
import Model.Formula exposing (..)
import Model.Scroll exposing (..)
import Model.Session exposing (..)
import Task
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
    | Rename Id String
    | StartRenaming Id
    | CommitRenaming
    | CancelRenaming
    | DragDropMsg DnDMsg
    | ResetSandbox SandboxID
    | HandleKeyboardEvent KeyboardEvent
    | HighlightOrigin (Maybe Id)
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

        ( newModel, nextCmd ) =
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
                    in
                    ( setSession dragId.route { session | actionMode = newMode } model, Cmd.none )

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

                                action =
                                    drop
                                        |> Maybe.andThen
                                            (\{ destination } ->
                                                case ( session.actionMode, destination ) of
                                                    ( ProofMode Justifying, DropNode target ) ->
                                                        Just (Deiterate source target)

                                                    ( ProofMode Justifying, DropLocation location ) ->
                                                        Just (Iterate source location)

                                                    ( EditMode _, DropLocation location ) ->
                                                        Just (Reorder source location.pos)

                                                    _ ->
                                                        Nothing
                                            )

                                ( modelApplied, applyCmd ) =
                                    case action of
                                        Just action_ ->
                                            update (Apply drag.route action_) model

                                        Nothing ->
                                            ( model, Cmd.none )

                                sessionApplied =
                                    getSession drag.route modelApplied
                            in
                            ( setSession drag.route { sessionApplied | actionMode = defaultMode } modelApplied
                            , applyCmd
                            )

                        -- Dragging
                        Nothing ->
                            ( model, Cmd.none )
    in
    ( { newModel | dragDrop = newDragDrop }, Cmd.batch [ cmd, nextCmd ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Apply route action ->
            let
                newSession =
                    apply (Debug.log "Action" action) (getSession route model)

                cmd =
                    case newSession.renaming of
                        Just _ ->
                            focusRenamingInput

                        Nothing ->
                            Cmd.none
            in
            ( setSessionWithHistory route newSession model, cmd )

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
            ( setSessionWithHistory model.playground.route (execAll model.playground) model, Cmd.none )

        Step ->
            Debug.todo "Action stepping not implemented yet"

        ChangeActionMode mode ->
            ( { model | playground = changeActionMode mode model.playground }, Cmd.none )

        ChangeExecMode mode ->
            ( { model | playground = changeExecMode mode model.playground }, Cmd.none )

        ToggleRecording recording ->
            ( { model | playground = toggleRecording recording model.playground }, Cmd.none )

        Undo ->
            ( undo model, Cmd.none )

        Redo ->
            ( redo model, Cmd.none )

        Auto ->
            Debug.todo "Auto not implemented yet"

        UpdateNewAtomName name ->
            ( { model | playground = updateNewAtomName name model.playground }, Cmd.none )

        Rename id newName ->
            let
                session =
                    model.playground

                newNet =
                    updateName id (\_ -> newName) session.net

                newSession =
                    { session | net = newNet }
            in
            ( { model | playground = newSession }, Cmd.none )

        StartRenaming id ->
            let
                session =
                    model.playground

                currentName =
                    getName id session.net

                newSession =
                    { session | renaming = Just { id = id, originalName = currentName } }
            in
            ( { model | playground = newSession }, focusRenamingInput )

        CommitRenaming ->
            let
                session =
                    model.playground

                newSession =
                    { session | renaming = Nothing }
            in
            ( { model | playground = newSession }, focusApp )

        CancelRenaming ->
            let
                session =
                    model.playground

                newSession =
                    case session.renaming of
                        Just { id, originalName } ->
                            let
                                newNet =
                                    updateName id (\_ -> originalName) session.net
                            in
                            { session
                                | net = newNet
                                , renaming = Nothing
                            }

                        Nothing ->
                            session
            in
            ( { model | playground = newSession }, focusApp )

        DragDropMsg dndMsg ->
            handleDragDropMsg dndMsg model

        ResetSandbox id ->
            ( { model | manualExamples = resetSandbox id model.manualExamples }
            , Cmd.none
            )

        HandleKeyboardEvent { ctrlKey, altKey, metaKey, shiftKey, key, keyCode } ->
            case ( ( ctrlKey || metaKey, altKey, shiftKey ), keyCode, key ) of
                ( ( True, _, _ ), _, Just "z" ) ->
                    update Undo model

                ( ( True, _, _ ), _, Just "y" ) ->
                    update Redo model

                ( ( True, _, _ ), Keyboard.Key.R, _ ) ->
                    update (ToggleRecording (not model.playground.recording)) model

                ( ( _, True, False ), Keyboard.Key.Tab, _ ) ->
                    let
                        newMode =
                            case model.playground.actionMode of
                                ProofMode _ ->
                                    defaultEditMode

                                _ ->
                                    ProofMode Interacting
                    in
                    update (ChangeActionMode newMode) model

                ( ( _, True, True ), Keyboard.Key.Tab, _ ) ->
                    update (ChangeExecMode (flipExecMode model.playground.execMode)) model

                ( ( _, True, _ ), Keyboard.Key.P, _ ) ->
                    update (ChangeActionMode (ProofMode Interacting)) model

                ( ( _, True, _ ), Keyboard.Key.E, _ ) ->
                    update (ChangeActionMode defaultEditMode) model

                ( ( _, True, _ ), Keyboard.Key.N, _ ) ->
                    update (ChangeActionMode NavigationMode) model

                ( ( _, True, _ ), Keyboard.Key.B, _ ) ->
                    update (ChangeExecMode Backward) model

                ( ( _, True, _ ), Keyboard.Key.F, _ ) ->
                    update (ChangeExecMode Forward) model

                ( ( _, _, _ ), _, Just "Enter" ) ->
                    update CommitRenaming model

                ( ( _, _, _ ), _, Just "Escape" ) ->
                    update CancelRenaming model

                _ ->
                    ( model, Cmd.none )

        HighlightOrigin maybeId ->
            let
                session =
                    model.playground

                newSession =
                    { session | hoveredOrigin = maybeId }
            in
            ( { model | playground = newSession }, Cmd.none )

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


focusRenamingInput : Cmd Msg
focusRenamingInput =
    Browser.Dom.focus "renaming-input"
        |> Task.attempt (\_ -> DoNothing)


focusApp : Cmd Msg
focusApp =
    Task.attempt (\_ -> DoNothing) (Browser.Dom.focus "app-container")
