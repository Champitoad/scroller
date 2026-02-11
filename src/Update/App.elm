port module Update.App exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Deque
import Html5.DragDrop as DnD
import Json.Decode exposing (Value)
import Keyboard.Event exposing (KeyboardEvent)
import Model.App exposing (..)
import Model.Formula exposing (..)
import Model.Scroll exposing (..)
import Model.Session exposing (..)
import Task
import Url
import View.Route as Route


type alias DetailedKeyboardEvent =
    { event : KeyboardEvent
    , code : Maybe String
    }


port dragstart : Value -> Cmd msg


type Msg
    = Apply Route Action
    | Transform Route (Session -> Session)
    | Exec Route Int
    | ExecAll
    | Step
    | ChangeActionMode ActionMode
    | ChangeOperationMode OperationMode
    | ChangeInteractionMode InteractionMode
    | ChangeExecMode ExecMode
    | ToggleRecording Bool
    | ToggleCopyMode Bool
    | ToggleInsertionMode Bool
    | Undo
    | Redo
    | Auto
    | UpdateNewAtomName String
    | SetNewAtomInputFocus Bool
    | Rename Id String
    | StartRenaming Id
    | CommitRenaming
    | CancelRenaming
    | DragDropMsg DnDMsg
    | SetDragModifiers { alt : Bool }
    | SetSelectionMode Bool
    | ClickedSelectionToggle Bool
    | ToggleSelection Id
    | ClearSelection
    | SetStepButtonFocus Bool
    | ResetSandbox SandboxID
    | HandleKeyboardEvent DetailedKeyboardEvent
    | HandleKeyUpEvent KeyboardEvent
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
                                ProofMode modeData ->
                                    ProofMode { modeData | interaction = Justifying modeData.copyMode }

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
                                        ProofMode modeData ->
                                            ProofMode { modeData | interaction = Interacting }

                                        EditMode modeData ->
                                            EditMode { modeData | interaction = Operating }

                                        _ ->
                                            session.actionMode

                                action =
                                    drop
                                        |> Maybe.andThen
                                            (\{ destination } ->
                                                case ( session.actionMode, destination ) of
                                                    ( ProofMode { copyMode }, DropNode target ) ->
                                                        if copyMode == Deiteration then
                                                            Just (Deiterate source target)

                                                        else
                                                            Nothing

                                                    ( ProofMode { copyMode }, DropLocation location ) ->
                                                        if copyMode == Iteration then
                                                            Just (Iterate source location)

                                                        else
                                                            Nothing

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
    ( { newModel | dragDrop = newDragDrop }, Cmd.batch [ cmd, nextCmd, focusApp ] )


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
                            focusApp
            in
            ( setSessionWithHistory route newSession model, cmd )

        Transform route transform ->
            let
                newSession =
                    transform (getSession route model)
            in
            ( setSessionWithHistory route newSession model, focusApp )

        Exec route actionIdx ->
            let
                newSession =
                    execute actionIdx (getSession route model)
            in
            ( setSessionWithHistory route newSession model, focusApp )

        ExecAll ->
            ( setSessionWithHistory model.playground.route (execAll model.playground) model, focusApp )

        Step ->
            case model.playground |> getActionsDeque model.playground.execMode |> Deque.popFront of
                ( Just _, _ ) ->
                    update (Exec Playground 0) model

                ( Nothing, _ ) ->
                    -- Should never happen since the step should be disabled
                    ( model, Cmd.none )

        ChangeActionMode mode ->
            ( { model | playground = changeActionMode mode model.playground }, focusApp )

        ChangeOperationMode mode ->
            ( { model | playground = changeOperationMode mode model.playground }, focusApp )

        ChangeInteractionMode mode ->
            ( { model | playground = changeInteractionMode mode model.playground }, focusApp )

        ChangeExecMode mode ->
            ( { model | playground = changeExecMode mode model.playground }, focusApp )

        ToggleRecording recording ->
            ( { model | playground = toggleRecording recording model.playground }, focusApp )

        Undo ->
            ( undo model, focusApp )

        Redo ->
            ( redo model, focusApp )

        Auto ->
            Debug.todo "Auto not implemented yet"

        UpdateNewAtomName name ->
            ( { model | playground = updateNewAtomName name model.playground }, Cmd.none )

        SetNewAtomInputFocus focused ->
            let
                session =
                    model.playground

                newSession =
                    setNewAtomInputFocus focused session
            in
            ( { model | playground = newSession }, Cmd.none )

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

        SetDragModifiers { alt } ->
            case model.playground.actionMode of
                EditMode _ ->
                    update (ToggleInsertionMode alt) model

                _ ->
                    update DoNothing model

        ResetSandbox id ->
            ( { model | manualExamples = resetSandbox id model.manualExamples }
            , Cmd.none
            )

        SetSelectionMode mode ->
            ( setSession Playground (setSelectionMode mode model.playground) model, focusApp )

        ClickedSelectionToggle mode ->
            let
                session =
                    model.playground

                newSession =
                    if mode then
                        setSelectionMode True session

                    else
                        setSelectionMode False session
            in
            ( setSession Playground newSession model, Cmd.none )

        ClearSelection ->
            ( setSession Playground (clearSelection model.playground) model, Cmd.none )

        ToggleSelection id ->
            ( setSession Playground (toggleSelection id model.playground) model, Cmd.none )

        SetStepButtonFocus focused ->
            let
                session =
                    model.playground

                newSession =
                    { session | stepButtonFocused = focused }
            in
            ( setSession Playground newSession model, Cmd.none )

        ToggleCopyMode doit ->
            ( setSession Playground (toggleCopyMode doit model.playground) model, Cmd.none )

        ToggleInsertionMode doit ->
            ( setSession Playground (toggleInsertionMode doit model.playground) model, Cmd.none )

        HandleKeyUpEvent { key } ->
            case key of
                Just "Alt" ->
                    case model.playground.actionMode of
                        EditMode _ ->
                            update (ToggleInsertionMode False) model

                        ProofMode _ ->
                            update (ToggleCopyMode False) model

                        _ ->
                            ( model, Cmd.none )

                Just "Shift" ->
                    update (SetSelectionMode False) model

                _ ->
                    ( model, Cmd.none )

        HandleKeyboardEvent { event, code } ->
            let
                { ctrlKey, altKey, shiftKey, key } =
                    event

                noModifiers =
                    not ctrlKey && not altKey && not shiftKey
            in
            case ( ( ctrlKey, altKey, shiftKey ), code, key ) of
                ( ( _, _, _ ), _, Just "Alt" ) ->
                    case model.playground.actionMode of
                        EditMode _ ->
                            update (ToggleInsertionMode True) model

                        ProofMode _ ->
                            update (ToggleCopyMode True) model

                        _ ->
                            ( model, Cmd.none )

                ( ( _, _, _ ), _, Just "Shift" ) ->
                    update (SetSelectionMode True) model

                ( ( True, False, False ), _, Just "z" ) ->
                    update Undo model

                ( ( True, False, False ), _, Just "y" ) ->
                    update Redo model

                ( ( _, _, _ ), _, Just "Tab" ) ->
                    if isTyping model.playground || not noModifiers then
                        ( model, Cmd.none )

                    else
                        update (ChangeExecMode (flipExecMode model.playground.execMode)) model

                ( ( _, _, _ ), Just "KeyR", _ ) ->
                    if isTyping model.playground || not noModifiers then
                        ( model, Cmd.none )

                    else
                        update (ToggleRecording (not model.playground.recording)) model

                ( ( _, _, _ ), Just "KeyQ", _ ) ->
                    if isTyping model.playground || not noModifiers then
                        ( model, Cmd.none )

                    else
                        update (ChangeActionMode defaultProofMode) model

                ( ( _, _, _ ), Just "KeyW", _ ) ->
                    if isTyping model.playground || not noModifiers then
                        ( model, Cmd.none )

                    else
                        update (ChangeActionMode defaultEditMode) model

                ( ( _, _, _ ), Just "KeyE", _ ) ->
                    if isTyping model.playground || not noModifiers then
                        ( model, Cmd.none )

                    else
                        update (ChangeActionMode NavigationMode) model

                ( ( _, _, _ ), _, Just "Enter" ) ->
                    update CommitRenaming model

                ( ( _, _, _ ), _, Just "Escape" ) ->
                    if shiftKey then
                        update ClearSelection model

                    else
                        update CancelRenaming model

                ( ( _, _, _ ), _, Just " " ) ->
                    if isTyping model.playground then
                        ( model, Cmd.none )

                    else
                        case model.playground.actionMode of
                            EditMode _ ->
                                ( { model | playground = toggleEditMode altKey model.playground }, focusApp )

                            ProofMode _ ->
                                ( { model | playground = toggleProofMode model.playground }, focusApp )

                            _ ->
                                ( model, Cmd.none )

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
