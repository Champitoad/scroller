module Model.Program exposing (..)

import Dict exposing (Dict)
import Iddict exposing (Iddict)
import Model.Scroll as Scroll exposing (..)
import Queue exposing (Queue)
import Utils.Maybe



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
   contexts in a program that she has visited.
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



-- Programs


type alias SandboxID =
    String


type Route
    = Playground
    | Manual SandboxID



{- A `Program` is made of the following data:

   - `net`: the whole net the user is working on

   - `navigation`: the navigation history

   - `route`: a unique identifier determining where the program appears in the app

   - `actionMode`: the current mode determining which actions can be performed through direct manipulation

   - `execMode`: the current mode determining in which direction actions are executed

   - `recording`: a boolean determining whether actions are recorded

   - `actions`: dictionary of all recorded actions pending for execution, accessed through unique IDs

   - `actionsQueue`: queue of recorded actions IDs
-}


type alias Program =
    { net : Net
    , navigation : Navigation
    , route : Route
    , actionMode : ActionMode
    , execMode : ExecMode
    , recording : Bool
    , actions : Iddict Action
    , actionsQueue : Queue Int
    }


fromNet : Net -> Program
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


map : (Net -> Net) -> Program -> Program
map f program =
    { program | net = f program.net }


updateNewAtomName : String -> Program -> Program
updateNewAtomName name program =
    let
        updatedMode =
            case program.actionMode of
                EditMode modeData ->
                    EditMode { modeData | newAtomName = name }

                mode ->
                    mode
    in
    { program | actionMode = updatedMode }



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


boundary : Program -> Net
boundary program =
    let
        boundaryFunc =
            case program.execMode of
                Forward ->
                    conclusion

                Backward ->
                    premiss
    in
    boundaryFunc program.net


execAll : Program -> Program
execAll program =
    { program
        | net = boundary program
        , actions = Iddict.empty
        , actionsQueue = Queue.empty
    }


changeActionMode : ActionMode -> Program -> Program
changeActionMode mode program =
    { program | actionMode = mode }


changeExecMode : ExecMode -> Program -> Program
changeExecMode mode program =
    { program | execMode = mode }


toggleRecording : Bool -> Program -> Program
toggleRecording recording program =
    { program | recording = recording }


flipExecMode : ExecMode -> ExecMode
flipExecMode execMode =
    case execMode of
        Forward ->
            Backward

        Backward ->
            Forward


isErased : Id -> Program -> Bool
isErased id program =
    case program.execMode of
        Forward ->
            isEliminated id program.net

        Backward ->
            isIntroduced id program.net


isErasedContext : Context -> Program -> Bool
isErasedContext ctx program =
    case program.execMode of
        Forward ->
            isEliminatedContext ctx program.net

        Backward ->
            isIntroducedContext ctx program.net


applicable : Program -> Action -> Result ActionError ()
applicable program action =
    let
        creationPolarity =
            case program.execMode of
                Forward ->
                    Neg

                Backward ->
                    Pos

        destructionPolarity =
            invert creationPolarity
    in
    case action of
        Open loc ->
            if isErasedContext loc.ctx program then
                Err Erased

            else
                Ok ()

        Close id ->
            if isErased id program then
                Err Erased

            else if getOutloop id (boundary program) /= Scroll.empty then
                Err (NonEmptyOutloop id)

            else if Dict.size (getInteractions id (boundary program)) /= 1 then
                Err (NonSingleInloop id)

            else
                Ok ()

        Insert loc _ ->
            if getPolarityContext loc.ctx program.net /= creationPolarity then
                Err InvalidPolarity

            else if isErasedContext loc.ctx program then
                Err Erased

            else
                Ok ()

        Delete id ->
            if getPolarity id program.net /= destructionPolarity then
                Err InvalidPolarity

            else if isErased id program then
                Err Erased

            else
                Ok ()

        Iterate src dst ->
            if getPolarityContext dst.ctx program.net /= creationPolarity then
                Err InvalidPolarity

            else if not (spans (getContext src program.net) dst.ctx program.net) then
                Err (OutOfScope src)

            else if isErased src program || isErasedContext dst.ctx program then
                Err Erased

            else
                Ok ()

        Deiterate src tgt ->
            if getPolarity tgt program.net /= destructionPolarity then
                Err InvalidPolarity

            else if not (spans (getContext src program.net) (getContext tgt program.net) program.net) then
                Err (OutOfScope src)

            else if isErased src program || isErased tgt program then
                Err Erased

            else if getSubnet src (boundary program) /= getSubnet tgt (boundary program) then
                Err (IncompatibleBoundaries src tgt)

            else
                Ok ()

        Reorder id pos ->
            Debug.todo "Reorder action not implemented yet"

        Decompose _ ->
            Ok ()


annotateExpansion : ExecMode -> Interaction -> Interaction
annotateExpansion execMode =
    case execMode of
        Forward ->
            makeOpened

        Backward ->
            makeClosed



{- `record action program` records `action` in `program` by:
   - generating a new ID `id` and associating `action` to `id` in `program.actions`
   - pushing `id` in `program.actionsQueue`
   - decorating the scroll net `program.net` with the justification/interaction corresponding to `action`
   - returning `id` for later usage (typically with `Program.execute`)

   This assumes that the action is indeed applicable in the program.
-}


record : Action -> Program -> ( Int, Program )
record action program =
    let
        ( actionId, newActions ) =
            Iddict.insert action program.actions

        newActionsQueue =
            Queue.enqueue actionId program.actionsQueue

        updatedNet : Net
        updatedNet =
            case action of
                Open loc ->
                    let
                        inloopName =
                            "Return"

                        scrollNet =
                            curl empty [ empty ]
                                |> updateName (baseId 0) (\_ -> inloopName)
                                |> updateInteraction (baseId 0) (annotateExpansion program.execMode)
                    in
                    graft loc scrollNet program.net

                Close id ->
                    updateInteraction id (annotateExpansion (flipExecMode program.execMode)) program.net

                Insert loc tok ->
                    insert loc tok program.net

                Delete id ->
                    delete id program.net

                Iterate src dst ->
                    iterate src dst program.net

                Deiterate src tgt ->
                    deiterate src tgt program.net

                Reorder id pos ->
                    Debug.todo "Reorder action not implemented yet"

                Decompose id ->
                    let
                        formNet =
                            case getShape id program.net of
                                Formula form ->
                                    form |> structOfFormula |> netOfStruct

                                _ ->
                                    getSubnet id program.net

                        loc =
                            getLocation id program.net
                    in
                    program.net
                        |> prune id
                        |> graft loc formNet
    in
    ( actionId
    , { program
        | actions = newActions
        , actionsQueue = newActionsQueue
        , net = updatedNet
      }
    )



{- `execute actionId program` executes the action with ID `actionId` in `program` by:
   - deleting the associated entry in `program.actions`
   - deleting `actionId` from `program.actionsQueue` if not already done
   - applying the semantics of the action in `program.net`
-}


execute : Int -> Program -> Program
execute actionId program =
    case Iddict.get actionId program.actions of
        Just action ->
            let
                newActions =
                    Iddict.remove actionId program.actions

                newActionsQueue =
                    Queue.filter (\id -> id /= actionId) program.actionsQueue

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
            { program
                | actions = newActions
                , actionsQueue = newActionsQueue
                , net = newFocus
            }

        Nothing ->
            Debug.log
                "Error: trying to execute action with non-existing ID. Returning the program unchanged."
                program


apply : Action -> Program -> Program
apply action program =
    let
        ( actionId, newProgram ) =
            record action program
    in
    if program.recording then
        newProgram

    else
        execute actionId program



-- A Sandbox is a Program that can be reset


type alias Sandbox =
    { initialProgram : Program
    , currentProgram : Program
    }


type alias Sandboxes =
    Dict SandboxID Sandbox


mkSandbox : Program -> Sandbox
mkSandbox program =
    { initialProgram = program
    , currentProgram = program
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


updateSandbox : SandboxID -> Program -> Sandboxes -> Sandboxes
updateSandbox id program sandboxes =
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
                    { initialProgram = sandbox.initialProgram
                    , currentProgram = program
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
            updateSandbox id sandbox.initialProgram sandboxes


resetAllSandboxes : Sandboxes -> Sandboxes
resetAllSandboxes sandboxes =
    Dict.map (\_ sb -> { sb | currentProgram = sb.initialProgram }) sandboxes


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
