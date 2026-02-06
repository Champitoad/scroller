module Model.Session exposing (..)

import Dict exposing (Dict)
import Iddict exposing (Iddict)
import Model.Scroll as Scroll exposing (..)
import Queue exposing (Queue)
import Utils.List
import Utils.State as State



-- Selection


type alias Selection =
    List Id



-- Modal UI


type CopyMode
    = Iteration
    | Deiteration


type InteractionMode
    = Expansion
    | Collapse


type ProofInteraction
    = Interacting
    | Justifying CopyMode -- Drag-and-Drop, store the copy mode defined before starting drag


type OperationMode
    = OInsertion
    | IInsertion
    | Deletion


type EditInteraction
    = Operating
    | Reordering -- Drag-and-Drop


type ActionMode
    = ProofMode
        { interaction : ProofInteraction
        , copyMode : CopyMode
        , interactionMode : InteractionMode
        }
    | EditMode
        { interaction : EditInteraction
        , operationMode : OperationMode
        , newAtomName : String
        , newAtomInputFocused : Bool
        , insertions : Dict Id Int -- maps node IDs to corresponding insertion action IDs
        }
    | NavigationMode


defaultProofMode : ActionMode
defaultProofMode =
    ProofMode
        { interaction = Interacting
        , interactionMode = Expansion
        , copyMode = Iteration
        }


defaultEditMode : ActionMode
defaultEditMode =
    EditMode
        { interaction = Operating
        , operationMode = OInsertion
        , newAtomName = ""
        , newAtomInputFocused = False
        , insertions = Dict.empty
        }


type ExecMode
    = Forward
    | Backward



-- Navigation
{- A `Navigation` is a non-empty sequence of visited `Context`s.

   It is used to keep track of the user's navigation history, i.e. the different
   contexts in a session that she has visited.
-}


type Navigation
    = Initial Context
    | Visit Context Navigation


initialNavigation : Navigation
initialNavigation =
    Initial TopLevel


current : Navigation -> Context
current navigation =
    case navigation of
        Visit context _ ->
            context

        Initial context ->
            context


changeFocus : Context -> Navigation -> Navigation
changeFocus context navigation =
    case navigation of
        Visit _ hist ->
            Visit context hist

        Initial _ ->
            Initial context


backtrack : Navigation -> Navigation
backtrack navigation =
    case navigation of
        Initial _ ->
            navigation

        Visit _ hist ->
            hist



-- Sessions


type alias SandboxID =
    String


type Route
    = Playground
    | Manual SandboxID



{- A `Session` is made of the following data:

   - `net`: the whole net the user is working on

   - `navigation`: the navigation history

   - `route`: a unique identifier determining where the session appears in the app

   - `actionMode`: the current mode determining which actions can be performed through direct manipulation

   - `execMode`: the current mode determining in which direction actions are executed

   - `recording`: a boolean determining whether actions are recorded

   - `actions`: dictionary of all recorded actions pending for execution, accessed through unique IDs

   - `actionsQueue`: queue of recorded actions IDs

   - `renaming`: the state of the renaming interaction, if any (ID of the node and its original name)
-}


type alias Session =
    { net : Net
    , navigation : Navigation
    , route : Route
    , actionMode : ActionMode
    , execMode : ExecMode
    , recording : Bool
    , actions : Iddict Action
    , actionsQueue : Queue Int
    , renaming : Maybe { id : Id, originalName : String }
    , hoveredOrigin : Maybe Id
    }


fromNet : Net -> Session
fromNet net =
    { net = net
    , navigation = initialNavigation
    , route = Playground
    , actionMode = defaultProofMode
    , execMode = Forward
    , recording = True
    , actions = Iddict.empty
    , actionsQueue = Queue.empty
    , renaming = Nothing
    , hoveredOrigin = Nothing
    }


map : (Net -> Net) -> Session -> Session
map f session =
    { session | net = f session.net }


updateNewAtomName : String -> Session -> Session
updateNewAtomName name session =
    let
        updatedMode =
            case session.actionMode of
                EditMode modeData ->
                    EditMode { modeData | newAtomName = name }

                mode ->
                    mode
    in
    { session | actionMode = updatedMode }


setNewAtomInputFocus : Bool -> Session -> Session
setNewAtomInputFocus focused session =
    let
        updatedMode =
            case session.actionMode of
                EditMode modeData ->
                    EditMode { modeData | newAtomInputFocused = focused }

                mode ->
                    mode
    in
    { session | actionMode = updatedMode }


isTyping : Session -> Bool
isTyping session =
    case session.actionMode of
        EditMode { newAtomInputFocused } ->
            newAtomInputFocused || (session.renaming /= Nothing)

        _ ->
            session.renaming /= Nothing



-- Actions


type
    Action
    -- Open a scroll with an empty outloop and a single empty inloop
    = Open Location
      -- Close a scroll with an empty outloop and a single inloop
    | Close Id
      -- Insert a token
    | Insert Location IToken
      -- Delete a node
    | Delete Id
      -- Iterate a node
    | Iterate Id Location
      -- Deiterate a node
    | Deiterate Id Id
      -- Reposition a node in the same area
    | Reorder Id Int
      -- Decompose a symbolic formula into the corresponding scroll structure
    | Decompose Id


type ActionError
    = InvalidPolarity
    | Erased
    | NonEmptyOutloop Id
    | NonSingleInloop Id
    | OutOfScope Id
    | IncompatibleBoundaries Id Id


boundary : Session -> Net
boundary session =
    let
        boundaryFunc =
            case session.execMode of
                Forward ->
                    conclusion

                Backward ->
                    premiss
    in
    boundaryFunc session.net


execAll : Session -> Session
execAll session =
    { session
        | net = boundary session
        , actions = Iddict.empty
        , actionsQueue = Queue.empty
        , actionMode =
            case session.actionMode of
                EditMode _ ->
                    defaultEditMode

                _ ->
                    session.actionMode
    }


changeActionMode : ActionMode -> Session -> Session
changeActionMode mode session =
    let
        newSession =
            commitInsertions session
    in
    { newSession | actionMode = mode }


changeOperationMode : OperationMode -> Session -> Session
changeOperationMode mode session =
    let
        newActionMode =
            case session.actionMode of
                EditMode modeData ->
                    EditMode { modeData | operationMode = mode }

                _ ->
                    session.actionMode
    in
    { session | actionMode = newActionMode }


changeInteractionMode : InteractionMode -> Session -> Session
changeInteractionMode mode session =
    let
        newActionMode =
            case session.actionMode of
                ProofMode modeData ->
                    ProofMode { modeData | interactionMode = mode }

                _ ->
                    session.actionMode
    in
    { session | actionMode = newActionMode }


changeExecMode : ExecMode -> Session -> Session
changeExecMode mode session =
    { session | execMode = mode }


toggleRecording : Bool -> Session -> Session
toggleRecording recording session =
    { session | recording = recording }


flipExecMode : ExecMode -> ExecMode
flipExecMode execMode =
    case execMode of
        Forward ->
            Backward

        Backward ->
            Forward


flipCopyMode : CopyMode -> CopyMode
flipCopyMode copyMode =
    case copyMode of
        Iteration ->
            Deiteration

        Deiteration ->
            Iteration


toggleCopyMode : Bool -> Session -> Session
toggleCopyMode isAlt session =
    case session.actionMode of
        ProofMode modeData ->
            { session
                | actionMode =
                    ProofMode
                        { modeData
                            | copyMode =
                                if isAlt then
                                    Deiteration

                                else
                                    Iteration
                        }
            }

        _ ->
            session


toggleInsertionMode : Bool -> Session -> Session
toggleInsertionMode isAlt session =
    case session.actionMode of
        EditMode ({ operationMode } as modeData) ->
            let
                newOpMode =
                    case operationMode of
                        Deletion ->
                            Deletion

                        _ ->
                            if isAlt then
                                IInsertion

                            else
                                OInsertion
            in
            { session
                | actionMode =
                    EditMode { modeData | operationMode = newOpMode }
            }

        _ ->
            session


toggleEditMode : Bool -> Session -> Session
toggleEditMode isAlt session =
    case session.actionMode of
        EditMode ({ operationMode } as modeData) ->
            let
                newOpMode =
                    case operationMode of
                        Deletion ->
                            if isAlt then
                                IInsertion

                            else
                                OInsertion

                        _ ->
                            Deletion
            in
            { session
                | actionMode =
                    EditMode { modeData | operationMode = newOpMode }
            }

        _ ->
            session


toggleProofMode : Session -> Session
toggleProofMode session =
    case session.actionMode of
        ProofMode ({ interactionMode } as modeData) ->
            let
                newInteractionMode =
                    case interactionMode of
                        Expansion ->
                            Collapse

                        Collapse ->
                            Expansion
            in
            { session
                | actionMode =
                    ProofMode { modeData | interactionMode = newInteractionMode }
            }

        _ ->
            session


isForward : ExecMode -> Bool
isForward execMode =
    case execMode of
        Forward ->
            True

        Backward ->
            False


isErased : Id -> Session -> Bool
isErased id session =
    case session.execMode of
        Forward ->
            isEliminated id session.net

        Backward ->
            isIntroduced id session.net


isErasedContext : Context -> Session -> Bool
isErasedContext ctx session =
    case session.execMode of
        Forward ->
            isEliminatedContext ctx session.net

        Backward ->
            isIntroducedContext ctx session.net


isInserted : Id -> Session -> Bool
isInserted id { actionMode } =
    case actionMode of
        EditMode { insertions } ->
            Dict.member id insertions

        _ ->
            False


commitInsertions : Session -> Session
commitInsertions session =
    { session
        | actions =
            case session.actionMode of
                EditMode { insertions } ->
                    Dict.foldl
                        (\ancId insertionActionId acc ->
                            let
                                newToken =
                                    boundary session
                                        |> buildTree ancId
                                        |> tokenOfTree
                            in
                            Iddict.update
                                insertionActionId
                                (Maybe.map
                                    (\action ->
                                        case action of
                                            Insert ancLoc _ ->
                                                Insert ancLoc newToken

                                            _ ->
                                                action
                                    )
                                )
                                acc
                        )
                        session.actions
                        insertions

                _ ->
                    session.actions
    }


insertionPolarity : ExecMode -> Polarity
insertionPolarity execMode =
    case execMode of
        Forward ->
            Neg

        Backward ->
            Pos


deletionPolarity : ExecMode -> Polarity
deletionPolarity =
    invert << insertionPolarity


applicable : Action -> Session -> Result ActionError ()
applicable action session =
    case action of
        Open loc ->
            if isErasedContext loc.ctx session then
                Err Erased

            else
                Ok ()

        Close id ->
            if isErased id session then
                Err Erased

            else if getOutloop id (boundary session) /= [] then
                Err (NonEmptyOutloop id)

            else if Dict.size (getOutloopInteractions id (boundary session)) /= 1 then
                Err (NonSingleInloop id)

            else
                Ok ()

        Insert loc _ ->
            if existsAncestorContext (\id -> isInserted id session) loc.ctx session.net then
                Ok ()

            else if getPolarityContext loc.ctx session.net /= insertionPolarity session.execMode then
                Err InvalidPolarity

            else if isErasedContext loc.ctx session then
                Err Erased

            else
                Ok ()

        Delete id ->
            if existsAncestor (\id_ -> isInserted id_ session) id session.net then
                Ok ()

            else if getPolarity id session.net /= deletionPolarity session.execMode then
                Err InvalidPolarity

            else if isErased id session then
                Err Erased

            else
                Ok ()

        Iterate src dst ->
            if getPolarityContext dst.ctx session.net /= deletionPolarity session.execMode then
                Err InvalidPolarity

            else if not (scopes src dst.ctx (boundary session)) then
                Err (OutOfScope src)

            else if isErased src session || isErasedContext dst.ctx session then
                Err Erased

            else
                Ok ()

        Deiterate src tgt ->
            if getPolarity tgt session.net /= insertionPolarity session.execMode then
                Err InvalidPolarity

            else if src == tgt || not (scopes src (getContext tgt session.net) (boundary session)) then
                Err (OutOfScope src)

            else if isErased src session || isErased tgt session then
                Err Erased

            else if not (isEqualIToken (getToken src (boundary session)) (getToken tgt (boundary session))) then
                Err (IncompatibleBoundaries src tgt)

            else
                Ok ()

        Reorder _ _ ->
            Ok ()

        Decompose _ ->
            Ok ()


annotateExpansion : ExecMode -> Interaction -> Interaction
annotateExpansion execMode =
    case execMode of
        Forward ->
            makeOpened

        Backward ->
            makeClosed


actionTransform : ExecMode -> Action -> Net -> Net
actionTransform execMode action =
    case action of
        Open loc ->
            let
                unopenedScrollTree =
                    emptyScroll |> hydrateOToken TopLevel Pos |> State.eval 0

                openedScroll =
                    unopenedScrollTree
                        |> dehydrateTree
                        |> updateInteraction 1 (annotateExpansion execMode)
            in
            graft loc openedScroll

        Close id ->
            \net ->
                case net |> Scroll.boundary (isForward execMode) |> getChildIds id |> List.filter (\cid -> isInloop cid net) of
                    [ inloopId ] ->
                        updateInteraction inloopId (annotateExpansion (flipExecMode execMode)) net

                    _ ->
                        net

        -- Do nothing, should never happen
        Insert loc tok ->
            insert True Nothing loc tok

        Delete id ->
            delete id

        Iterate src dst ->
            iterate (isForward execMode) src dst

        Deiterate src tgt ->
            deiterate (isForward execMode) src tgt

        Reorder id tgtPos ->
            \net ->
                let
                    srcPos =
                        getPosition id net

                    reorder =
                        Utils.List.move srcPos tgtPos
                in
                case getContext id net of
                    TopLevel ->
                        { nodes = net.nodes
                        , roots = reorder net.roots
                        }

                    Inside parentId ->
                        updateShape parentId
                            (\shape ->
                                case shape of
                                    Sep children interaction ->
                                        Sep (reorder children) interaction

                                    _ ->
                                        shape
                            )
                            net

        Decompose id ->
            \net ->
                let
                    formNet =
                        case getShape id net of
                            Formula form ->
                                form |> interpretFormula |> netOfStruct

                            _ ->
                                getSubnet id net

                    loc =
                        getLocation id net
                in
                net
                    |> prune id
                    |> graft loc formNet



{- `record action session` records `action` in `session` by:
   - generating a new ID `id` and associating `action` to `id` in `session.actions`
   - pushing `id` in `session.actionsQueue`
   - decorating the scroll net `session.net` with the justification/interaction corresponding to `action`
   - returning `id` for later usage (typically with `Session.execute`)

   This assumes that the action is indeed applicable in the session.
-}


record : Action -> Session -> ( Int, Session )
record action session =
    let
        ( actionId, newActions ) =
            Iddict.insert action session.actions

        updatedActionsQueue =
            Queue.enqueue actionId session.actionsQueue

        transformedNet =
            actionTransform session.execMode action session.net

        renamingData loc =
            let
                id =
                    getNodeIdAtLocation loc transformedNet

                defaultName =
                    getName id transformedNet

                netWithEmptyName =
                    updateName id (\_ -> "") transformedNet
            in
            ( id
            , netWithEmptyName
            , Just { id = id, originalName = defaultName }
            )

        ( finalNet, updatedActionMode, updatedRenaming ) =
            case ( session.actionMode, action ) of
                ( EditMode editData, Insert loc _ ) ->
                    let
                        ( id, netWithEmptyName, renaming ) =
                            renamingData loc
                    in
                    ( netWithEmptyName
                    , EditMode
                        { editData
                            | insertions =
                                Dict.insert
                                    id
                                    actionId
                                    editData.insertions
                        }
                    , renaming
                    )

                ( _, Iterate _ loc ) ->
                    let
                        ( _, netWithEmptyName, renaming ) =
                            renamingData loc
                    in
                    ( netWithEmptyName
                    , session.actionMode
                    , renaming
                    )

                _ ->
                    ( transformedNet, session.actionMode, Nothing )
    in
    ( actionId
    , { session
        | actions = newActions
        , actionsQueue = updatedActionsQueue
        , actionMode = updatedActionMode
        , net = finalNet
        , renaming = updatedRenaming
      }
    )



{- `execute actionId session` executes the action with ID `actionId` in `session` by:
   - deleting the associated entry in `session.actions`
   - deleting `actionId` from `session.actionsQueue` if not already done
   - applying the semantics of the action in `session.net`
-}


execute : Int -> Session -> Session
execute actionId session =
    case Iddict.get actionId session.actions of
        Just action ->
            let
                newActions =
                    Iddict.remove actionId session.actions

                newActionsQueue =
                    Queue.filter (\id -> id /= actionId) session.actionsQueue

                newFocus : Net
                newFocus =
                    case action of
                        Open _ ->
                            Debug.todo "Open action execution not implemented yet."

                        Close _ ->
                            Debug.todo "Close action execution not implemented yet."

                        Insert _ _ ->
                            Debug.todo "Insert action execution not implemented yet."

                        Delete _ ->
                            Debug.todo "Delete action execution not implemented yet."

                        Iterate _ _ ->
                            Debug.todo "Iterate action execution not implemented yet."

                        Deiterate _ _ ->
                            Debug.todo "Deiterate action execution not implemented yet."

                        Reorder _ _ ->
                            Debug.todo "Reorder action execution not implemented yet"

                        Decompose _ ->
                            Debug.todo "Decompose action execution not implemented yet."
            in
            { session
                | actions = newActions
                , actionsQueue = newActionsQueue
                , net = newFocus
            }

        Nothing ->
            let
                _ =
                    Debug.log
                        "Error: trying to execute action with non-existing ID. Returning the session unchanged."
            in
            session


apply : Action -> Session -> Session
apply action session =
    let
        ( actionId, newSession ) =
            record action session
    in
    if session.recording then
        newSession

    else
        execute actionId session



-- A Sandbox is a Session that can be reset


type alias Sandbox =
    { initialSession : Session
    , currentSession : Session
    }


type alias Sandboxes =
    Dict SandboxID Sandbox


mkSandbox : Session -> Sandbox
mkSandbox session =
    { initialSession = session
    , currentSession = session
    }


getSandbox : SandboxID -> Sandboxes -> Sandbox
getSandbox id sandboxes =
    case Dict.get id sandboxes of
        Nothing ->
            let
                _ =
                    Debug.log "Warning" "trying to retrieve a non-existing sandbox; returning a dummy one."
            in
            mkSandbox (fromNet empty)

        Just sandbox ->
            sandbox


updateSandbox : SandboxID -> Session -> Sandboxes -> Sandboxes
updateSandbox id session sandboxes =
    case Dict.get id sandboxes of
        Nothing ->
            let
                _ =
                    Debug.log "Warning" "trying to update a non-existing sandbox. Ignoring."
            in
            sandboxes

        Just sandbox ->
            let
                updatedSandbox =
                    { initialSession = sandbox.initialSession
                    , currentSession = session
                    }
            in
            Dict.insert id updatedSandbox sandboxes


resetSandbox : SandboxID -> Sandboxes -> Sandboxes
resetSandbox id sandboxes =
    case Dict.get id sandboxes of
        Nothing ->
            let
                _ =
                    Debug.log "Warning" "trying to reset a non-existing sandbox. Ignoring."
            in
            sandboxes

        Just sandbox ->
            updateSandbox id sandbox.initialSession sandboxes


resetAllSandboxes : Sandboxes -> Sandboxes
resetAllSandboxes sandboxes =
    Dict.map (\_ sb -> { sb | currentSession = sb.initialSession }) sandboxes


manualExamples : Sandboxes
manualExamples =
    let
        makeSandbox id actionMode execMode net =
            mkSandbox
                { net = net
                , navigation = initialNavigation
                , route = Manual id
                , actionMode = actionMode
                , execMode = execMode
                , recording = True
                , actions = Iddict.empty
                , actionsQueue = Queue.empty
                , renaming = Nothing
                , hoveredOrigin = Nothing
                }

        examples : List ( SandboxID, ActionMode, Net )
        examples =
            [ ( "Flower", defaultEditMode, netOfStruct [ curl [ a "a", a "b" ] [ [ a "c", a "d" ] ] ] )
            , ( "QED", defaultProofMode, netOfStruct [ curl [ a "a" ] [ [ emptyScroll ] ] ] )
            , ( "Justify", defaultProofMode, netOfStruct [ Scroll.identity ] )
            , ( "Unlock", defaultProofMode, netOfStruct [ curl [ curl [] [ [ a "a" ] ] ] [ [ a "a" ] ] ] )
            , ( "Import", defaultProofMode, netOfStruct [ Scroll.modusPonensCurryfied ] )
            , ( "Case"
              , defaultProofMode
              , netOfStruct [ curl [ curl [] [ [ a "a", a "b" ] ], curl [ a "a" ] [ [ a "c" ] ], curl [ a "b" ] [ [ a "c" ] ] ] [ [ a "c" ] ] ]
              )
            , ( "Decompose", defaultProofMode, netOfStruct [ Scroll.orElim ] )
            ]
    in
    examples
        |> List.map (\( id, mode, net ) -> ( id, makeSandbox id mode Forward net ))
        |> Dict.fromList
