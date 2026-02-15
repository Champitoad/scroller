module Model.Session exposing (..)

import Deque exposing (Deque)
import Dict exposing (Dict)
import List.Extra
import Model.Scroll as Scroll exposing (..)
import Set exposing (Set)
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
        , insertedNodes : Set Id -- maps node IDs to corresponding insertion action IDs
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
        , insertedNodes = Set.empty
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



-- Actions


type
    Action
    -- Open a scroll with an empty outloop and a single inloop around a selected contiguous sequence of nodes
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
    | NonEmptyInloop Id
    | NonEmptyOutloop Id
    | NonSingleInloop Id
    | OutOfScope Id
    | IncompatibleBoundaries Id Id
    | NonEmptySelection
    | NonContiguousSelection


type alias ActionDeque =
    Deque ( Action, Id )



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

   - `actions`: two double-ended queues for recording actions in Forward and Backward modes

   - `renaming`: the state of the renaming interaction, if any (ID of the node and its original name)

   - `hoveredOrigin`: ID of the origin of a hovered copy, if any

   - `selection`: list of selected nodes

   - `selecting`: whether the interface is in selection mode
-}


type alias Session =
    { net : Net
    , navigation : Navigation
    , route : Route
    , actionMode : ActionMode
    , execMode : ExecMode
    , recording : Bool
    , actions : { forward : ActionDeque, backward : ActionDeque }
    , renaming : Maybe { id : Id, originalName : String }
    , hoveredOrigin : Maybe Id
    , selection : Selection
    , selecting : Bool
    , stepButtonFocused : Bool
    }


fromNet : Net -> Session
fromNet net =
    { net = net
    , navigation = initialNavigation
    , route = Playground
    , actionMode = defaultProofMode
    , execMode = Forward
    , recording = True
    , actions = { forward = Deque.empty, backward = Deque.empty }
    , renaming = Nothing
    , hoveredOrigin = Nothing
    , selection = []
    , selecting = False
    , stepButtonFocused = False
    }


getHighlightedAction : Session -> Maybe ( Action, Id )
getHighlightedAction session =
    if session.stepButtonFocused then
        session |> getActionsDeque session.execMode |> getActionAtIndex 0

    else
        Nothing


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


setSelectionMode : Bool -> Session -> Session
setSelectionMode mode session =
    { session | selecting = mode }


clearSelection : Session -> Session
clearSelection session =
    { session | selection = [] }


toggleSelection : Id -> Session -> Session
toggleSelection id session =
    let
        newSelection =
            if List.member id session.selection then
                List.filter (\x -> x /= id) session.selection

            else
                id :: session.selection
    in
    { session | selection = newSelection }


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


boundaryToken : Id -> Session -> IToken
boundaryToken id session =
    Scroll.boundaryToken (isForward session.execMode) id session.net


execAll : Session -> Session
execAll session =
    { session
        | net = boundary session
        , actions = { forward = Deque.empty, backward = Deque.empty }
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
        EditMode { insertedNodes } ->
            Set.member id insertedNodes

        _ ->
            False


updateInsertion : Session -> Id -> ActionDeque -> ActionDeque
updateInsertion session ancId acc =
    let
        newToken =
            boundaryToken ancId session
    in
    acc
        |> Deque.map
            (\( action, locus ) ->
                case action of
                    Insert ancLoc _ ->
                        ( Insert ancLoc newToken, locus )

                    _ ->
                        ( action, locus )
            )


commitInsertions : Session -> Session
commitInsertions session =
    { session
        | actions =
            case session.actionMode of
                EditMode { insertedNodes } ->
                    { forward =
                        Set.foldl
                            (updateInsertion session)
                            session.actions.forward
                            insertedNodes
                    , backward =
                        Set.foldl
                            (updateInsertion session)
                            session.actions.backward
                            insertedNodes
                    }

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
                let
                    isSelectionContiguous () =
                        let
                            childIds =
                                getChildIdsContext loc.ctx session.net

                            sortedSelection =
                                List.sortBy (\id -> Scroll.getPosition id session.net) session.selection
                        in
                        List.Extra.isSubsequenceOf sortedSelection childIds
                in
                if not (List.isEmpty session.selection) then
                    if isForward session.execMode then
                        Err NonEmptySelection

                    else if not (isSelectionContiguous ()) then
                        Err NonContiguousSelection

                    else
                        Ok ()

                else
                    Ok ()

        Close id ->
            if isErased id session then
                Err Erased

            else if getOutloop id (boundary session) /= [] then
                Err (NonEmptyOutloop id)

            else
                case getOutloopInteractions id (boundary session) |> Dict.toList of
                    [ ( inloopId, _ ) ] ->
                        if session.execMode == Backward && not (List.isEmpty (getChildIds inloopId (boundary session))) then
                            Err (NonEmptyInloop inloopId)

                        else
                            Ok ()

                    _ ->
                        Err (NonSingleInloop id)

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

            else if not ((boundary session |> getToken src |> tokenOfIToken) == (boundary session |> getToken tgt |> tokenOfIToken)) then
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


actionTransform : Selection -> ExecMode -> Action -> Net -> ( Id, Net )
actionTransform selection execMode action =
    case action of
        Open loc ->
            if List.isEmpty selection then
                \net ->
                    let
                        openedScroll =
                            emptyScroll
                                |> hydrateOToken TopLevel Pos
                                |> State.eval 0
                                |> dehydrateTree
                                |> updateInteraction 1 (annotateExpansion execMode)

                        ( ids, newNet ) =
                            graft loc openedScroll net
                    in
                    -- Dirty trick here, we rely on the fact that inloop ID == outloop ID + 1 from
                    -- `hydrateOToken` implementation, as well as how `freshify` computes fresh IDs
                    -- by shifting, thus preserving the above equality
                    ( List.head ids |> Maybe.map ((+) 1) |> Maybe.withDefault -1, newNet )

            else
                \net ->
                    let
                        sortedSelection =
                            List.sortBy (\id -> Scroll.getPosition id net) selection

                        ( inloopId, netWithInloop ) =
                            Scroll.enclose sortedSelection (Just Scroll.attachment) net

                        ( _, netWithOutloop ) =
                            Scroll.enclose [ inloopId ] Nothing netWithInloop
                    in
                    ( inloopId, Scroll.updateInteraction inloopId (annotateExpansion execMode) netWithOutloop )

        Close id ->
            \net ->
                case net |> Scroll.boundary (isForward execMode) |> getChildIds id |> List.filter (\cid -> isInloop cid net) of
                    [ inloopId ] ->
                        ( inloopId, updateInteraction inloopId (annotateExpansion (flipExecMode execMode)) net )

                    _ ->
                        -- Should never happen since we only allow correction actions
                        ( id, net )

        Insert loc tok ->
            insert True Nothing loc tok

        Delete id ->
            \net -> ( id, delete id net )

        Iterate src dst ->
            iterate (isForward execMode) src dst

        Deiterate src tgt ->
            \net -> ( tgt, deiterate (isForward execMode) src tgt net )

        Reorder id tgtPos ->
            \net ->
                let
                    srcPos =
                        getPosition id net

                    reorder =
                        Utils.List.move srcPos tgtPos

                    newNet =
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
                in
                ( id, newNet )

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

                    ( ids, newNet ) =
                        net
                            |> prune id
                            |> graft loc formNet
                in
                ( List.head ids |> Maybe.withDefault -1, newNet )



{- `record action session` records `action` in `session` by:
   - enqueuing `action` and its dual in `session.actions`
   - decorating the scroll net `session.net` with the justification/interaction corresponding to `action`

   This assumes that the action is indeed applicable in the session.
-}


record : Action -> Session -> Session
record action session =
    let
        ( locus, transformedNet ) =
            actionTransform session.selection session.execMode action session.net

        dualAction =
            case action of
                Open _ ->
                    Close locus

                Close id ->
                    Open (getLocation id (boundary session))

                Insert _ _ ->
                    Delete locus

                Delete id ->
                    Insert (getLocation id (boundary session)) (boundaryToken id session)

                Iterate src _ ->
                    Deiterate src locus

                Deiterate src tgt ->
                    Iterate src (getLocation tgt (boundary session))

                Reorder id _ ->
                    Reorder id (getPosition id (boundary session))

                Decompose _ ->
                    Debug.todo "dualAction: Decompose"

        ( forwardPush, backwardPush ) =
            if isForward session.execMode then
                ( Deque.pushBack ( action, locus ), Deque.pushFront ( dualAction, locus ) )

            else
                ( Deque.pushFront ( dualAction, locus ), Deque.pushBack ( action, locus ) )

        newActions =
            { forward = forwardPush session.actions.forward
            , backward = backwardPush session.actions.backward
            }

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
                    , EditMode { editData | insertedNodes = Set.insert id editData.insertedNodes }
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
    { session
        | actions = newActions
        , actionMode = updatedActionMode
        , net = finalNet
        , renaming = updatedRenaming
    }


getActionsDeque : ExecMode -> Session -> ActionDeque
getActionsDeque execMode session =
    case execMode of
        Forward ->
            session.actions.forward

        Backward ->
            session.actions.backward


setActionsDeque : ExecMode -> ActionDeque -> Session -> Session
setActionsDeque execMode actionsDeque session =
    case execMode of
        Forward ->
            { session | actions = { forward = actionsDeque, backward = session.actions.backward } }

        Backward ->
            { session | actions = { backward = actionsDeque, forward = session.actions.forward } }


getActionAtIndex : Int -> ActionDeque -> Maybe ( Action, Id )
getActionAtIndex idx actionsDeque =
    actionsDeque
        |> Deque.toList
        |> List.Extra.getAt idx


removeActionAtIndex : Int -> ActionDeque -> ActionDeque
removeActionAtIndex idx actionsDeque =
    actionsDeque
        |> Deque.toList
        |> List.Extra.removeAt idx
        |> Deque.fromList


findIntroIndex : Session -> Id -> Maybe Int
findIntroIndex session id =
    getActionsDeque session.execMode session
        |> Deque.toList
        |> List.Extra.findIndex (\( _, locus ) -> locus == id)


findElimIndex : Session -> Id -> Maybe Int
findElimIndex session id =
    getActionsDeque session.execMode session
        |> Deque.toList
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, ( _, locus ) ) -> locus == id)
        |> List.map Tuple.first
        |> List.maximum



{- `execute actionIdx session` executes the action at index `actionIdx` in `session.actions` by:
   - removing the associated entry in `session.actions`
   - applying the semantics of the action in `session.net`
-}


execute : Int -> Session -> Session
execute actionIdx session =
    let
        currentActionsDeque =
            getActionsDeque session.execMode session

        oppositeActionsDeque =
            getActionsDeque (flipExecMode session.execMode) session
    in
    case currentActionsDeque |> getActionAtIndex actionIdx of
        Just ( action, locus ) ->
            let
                newCurrentActionsDeque =
                    removeActionAtIndex actionIdx currentActionsDeque

                newOppositeActionsDeque =
                    removeActionAtIndex (Deque.length oppositeActionsDeque - 1 - actionIdx) oppositeActionsDeque

                newNet : Net
                newNet =
                    case action of
                        Open _ ->
                            updateInteraction locus
                                (\int ->
                                    if isForward session.execMode then
                                        { int | opened = False }

                                    else
                                        { int | closed = False }
                                )
                                session.net

                        Close outloopId ->
                            List.foldl
                                (\id acc -> prune id acc)
                                (removeScrollNodes (isForward session.execMode) locus session.net)
                                (getChildIds outloopId session.net)

                        Insert _ _ ->
                            updateJustif locus (\justif -> { justif | self = False }) session.net

                        Delete _ ->
                            prune locus session.net

                        Iterate _ _ ->
                            List.foldl
                                (\subnodeId ->
                                    updateJustif subnodeId
                                        (\justif ->
                                            if subnodeId == locus then
                                                { justif | copy = Nothing }

                                            else
                                                { justif | subcopy = Nothing }
                                        )
                                )
                                session.net
                                (getDescendentIds locus session.net)

                        Deiterate _ _ ->
                            prune locus session.net

                        Reorder _ _ ->
                            Debug.todo "Reorder action execution not implemented yet"

                        Decompose _ ->
                            Debug.todo "Decompose action execution not implemented yet."
            in
            { session | net = newNet }
                |> setActionsDeque session.execMode newCurrentActionsDeque
                |> setActionsDeque (flipExecMode session.execMode) newOppositeActionsDeque

        Nothing ->
            let
                _ =
                    Debug.log
                        "Error: trying to execute non-existing action. Returning the session unchanged."
            in
            session


apply : Action -> Session -> Session
apply action session =
    let
        newSession =
            record action session
    in
    if session.recording then
        newSession

    else
        execute (Deque.length (getActionsDeque newSession.execMode newSession) - 1) newSession



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
                , actions = { forward = Deque.empty, backward = Deque.empty }
                , renaming = Nothing
                , hoveredOrigin = Nothing
                , selection = []
                , selecting = False
                , stepButtonFocused = False
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
