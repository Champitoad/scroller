module Model.Session exposing (..)

import Dict exposing (Dict)
import Iddict exposing (Iddict)
import Model.Scroll as Scroll exposing (..)
import Queue exposing (Queue)
import Utils.List



-- Selection


type alias Selection =
    List Id



-- Modal UI


type ProofInteraction
    = Interacting
    | Justifying -- Drag-and-Drop


type EditInteraction
    = Operating
    | Reordering -- Drag-and-Drop


type ActionMode
    = ProofMode ProofInteraction
    | EditMode
        { interaction : EditInteraction
        , newAtomName : String
        , insertions : Dict Id Int -- maps node IDs to corresponding insertion action IDs
        }
    | NavigationMode


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
    }


fromNet : Net -> Session
fromNet net =
    { net = net
    , navigation = initialNavigation
    , route = Playground
    , actionMode = ProofMode Interacting
    , execMode = Forward
    , recording = True
    , actions = Iddict.empty
    , actionsQueue = Queue.empty
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
    }


changeActionMode : ActionMode -> Session -> Session
changeActionMode mode session =
    let
        newSession =
            case session.actionMode of
                EditMode _ ->
                    commitInsertions session

                _ ->
                    session
    in
    { newSession | actionMode = mode }


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
isInserted id { net, execMode } =
    getPolarity id net == creationPolarity execMode && (getJustif id net).self


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


creationPolarity : ExecMode -> Polarity
creationPolarity execMode =
    case execMode of
        Forward ->
            Neg

        Backward ->
            Pos


destructionPolarity : ExecMode -> Polarity
destructionPolarity =
    invert << creationPolarity


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
            if getPolarityContext loc.ctx session.net /= creationPolarity session.execMode then
                Err InvalidPolarity

            else if isErasedContext loc.ctx session then
                Err Erased

            else
                Ok ()

        Delete id ->
            if getPolarity id session.net /= destructionPolarity session.execMode then
                Err InvalidPolarity

            else if isErased id session then
                Err Erased

            else
                Ok ()

        Iterate src dst ->
            if getPolarityContext dst.ctx session.net /= creationPolarity session.execMode then
                Err InvalidPolarity

            else if not (spans (getContext src session.net) dst.ctx session.net) then
                Err (OutOfScope src)

            else if isErased src session || isErasedContext dst.ctx session then
                Err Erased

            else
                Ok ()

        Deiterate src tgt ->
            if getPolarity tgt session.net /= destructionPolarity session.execMode then
                Err InvalidPolarity

            else if not (spans (getContext src session.net) (getContext tgt session.net) session.net) then
                Err (OutOfScope src)

            else if isErased src session || isErased tgt session then
                Err Erased

            else if getSubnet src (boundary session) /= getSubnet tgt (boundary session) then
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
            graft loc (emptyScrollNet (makeOpened attachment))

        Close id ->
            updateInteraction id (annotateExpansion (flipExecMode execMode))

        Insert loc tok ->
            insert True loc tok

        Delete id ->
            delete id

        Iterate src dst ->
            iterate src dst

        Deiterate src tgt ->
            deiterate src tgt

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
                                form |> structOfFormula |> netOfStruct

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

        updatedActionMode =
            case ( session.actionMode, action ) of
                ( EditMode editInteraction, Insert loc _ ) ->
                    EditMode
                        { editInteraction
                            | insertions =
                                Dict.insert (getNodeIdAtLocation loc session.net) actionId editInteraction.insertions
                        }

                _ ->
                    session.actionMode
    in
    ( actionId
    , { session
        | actions = newActions
        , actionsQueue = updatedActionsQueue
        , actionMode = updatedActionMode
        , net = transformedNet
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
                        Open loc ->
                            Debug.todo "Open action execution not implemented yet."

                        Close id ->
                            Debug.todo "Close action execution not implemented yet."

                        Insert loc tok ->
                            Debug.todo "Insert action execution not implemented yet."

                        Delete id ->
                            Debug.todo "Delete action execution not implemented yet."

                        Iterate src dst ->
                            Debug.todo "Iterate action execution not implemented yet."

                        Deiterate src tgt ->
                            Debug.todo "Deiterate action execution not implemented yet."

                        Reorder id pos ->
                            Debug.todo "Reorder action execution not implemented yet"

                        Decompose id ->
                            Debug.todo "Decompose action execution not implemented yet."
            in
            { session
                | actions = newActions
                , actionsQueue = newActionsQueue
                , net = newFocus
            }

        Nothing ->
            Debug.log
                "Error: trying to execute action with non-existing ID. Returning the session unchanged."
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
                }

        examples : List ( SandboxID, ActionMode, Net )
        examples =
            [ ( "Flower", ProofMode Interacting, curl (juxtaposeList [ a "a", a "b" ]) [ a "c", a "d" ] )
            , ( "QED", ProofMode Interacting, curl (a "a") [ empty ] )
            , ( "Justify", ProofMode Interacting, Scroll.identity )
            , ( "Unlock", ProofMode Interacting, curl (curl empty [ a "a" ]) [ a "a" ] )
            , ( "Import", ProofMode Interacting, Scroll.modusPonensCurryfied )
            , ( "Case"
              , ProofMode Interacting
              , curl (juxtaposeList [ curl empty [ a "a", a "b" ], curl (a "a") [ a "c" ], curl (a "b") [ a "c" ] ]) [ a "c" ]
              )
            , ( "Decompose", ProofMode Interacting, Scroll.orElim )
            ]
    in
    examples
        |> List.map (\( id, mode, net ) -> ( id, makeSandbox id mode Forward net ))
        |> Dict.fromList
