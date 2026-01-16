module Model.Program exposing (..)

import Dict exposing (Dict)
import Iddict exposing (Iddict)
import Model.Formula exposing (Formula)
import Model.Scroll as Scroll exposing (..)
import Queue exposing (Queue)
import Utils.Maybe



-- Selection


type alias Selection =
    List Id



-- Modal UI


type ProofInteraction
    = Interacting
    | Justifying


type EditInteraction
    = Operating
    | Reordering


type ActionMode
    = ProofMode ProofInteraction
    | EditMode
        { interaction : EditInteraction
        , newAtomName : String
        }
    | NavigationMode


type ExecMode
    = Forward
    | Backward



-- Navigation
{- A `Navigation` is a non-empty sequence of visited `Location`s.

   It is used to keep track of the user's navigation history, i.e. the different
   locations in a goal that she has visited.
-}


type Navigation
    = Initial Location
    | Visit Location Navigation


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

   `net`: the whole net the user is working on

   `navigation`: the navigation history

   `route`: a unique identifier determining where the goal appears in the app

   `actionMode`: the current mode determining which actions can be performed through direct manipulation

   `execMode`: the current mode determining in which direction actions are executed

   `recording`: a boolean determining whether actions are recorded

   `actions`: dictionary of all recorded actions pending for execution, accessed through unique IDs

   `actionsQueue`: queue of recorded actions IDs
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
map f goal =
    { goal | net = f goal.net }


updateNewAtomName : String -> Program -> Program
updateNewAtomName name goal =
    let
        newMode =
            case goal.actionMode of
                EditMode modeData ->
                    EditMode { modeData | newAtomName = name }

                mode ->
                    mode
    in
    { goal | actionMode = newMode }



-- Actions


type
    Action
    -- Open a scroll with an empty outloop and a single empty inloop
    = Open Location
      -- Close a scroll with an empty outloop and a single inloop
    | Close Id
      -- Insert a (possibly attached) token
    | Insert Location IToken
      -- Delete a node
    | Delete Id
      -- Iterate a node
    | Iterate Id Location
      -- Deiterate a node
    | Deiterate Id Id
      -- Decompose a symbolic formula into the corresponding scroll structure
    | Decompose Id


type ActionError
    = InvalidPolarity Polarity
    | Erased Id
    | InvalidBranchId Id Id
    | NonEmptyOutloop Id
    | NonSingleInloop Id
    | OutOfScope Id Id
    | IncompatibleBoundaries Id Id


boundaryVal : ExecMode -> Polarity -> Val -> Struct
boundaryVal execMode =
    let
        forwardBoundary pol =
            case pol of
                Pos ->
                    conclusionVal

                Neg ->
                    premissVal
    in
    case execMode of
        Forward ->
            forwardBoundary

        Backward ->
            invert >> forwardBoundary


boundary : ExecMode -> Polarity -> Net -> Struct
boundary execMode =
    let
        forwardBoundary pol =
            case pol of
                Pos ->
                    conclusion

                Neg ->
                    premiss
    in
    case execMode of
        Forward ->
            forwardBoundary

        Backward ->
            invert >> forwardBoundary


execAll : Program -> Program
execAll goal =
    { goal
        | net = netFromStruct (boundary goal.execMode Pos goal.net)
        , actions = Iddict.empty
        , actionsQueue = Queue.empty
    }


changeActionMode : ActionMode -> Program -> Program
changeActionMode mode goal =
    let
        newFocus =
            case mode of
                ProofMode _ ->
                    List.map commitVal goal.net

                _ ->
                    goal.net
    in
    { goal | actionMode = mode, net = newFocus }


changeExecMode : ExecMode -> Program -> Program
changeExecMode mode goal =
    { goal | execMode = mode }


toggleRecording : Bool -> Program -> Program
toggleRecording recording goal =
    { goal | recording = recording }


getSingleInloop : ExecMode -> Context -> ScrollVal -> Maybe ( Ident, Env )
getSingleInloop execMode ctx scroll =
    let
        erasedEnv =
            case execMode of
                Forward ->
                    eliminatedEnv

                Backward ->
                    introducedEnv

        nonErasedInloops =
            scroll.data.inloops
                |> Dict.filter (\id env -> not (erasedEnv ctx scroll id env))
                |> Dict.toList
    in
    case nonErasedInloops of
        [ inloop ] ->
            Just inloop

        _ ->
            Nothing


applicable : Program -> Action -> Result ActionError ()
applicable goal action =
    let
        { erasedVal, erasedScrollVal, erasedEnv, erasedArea } =
            case goal.execMode of
                Forward ->
                    { erasedVal = eliminatedVal
                    , erasedScrollVal = eliminatedScrollVal
                    , erasedEnv = eliminatedEnv
                    , erasedArea = eliminatedArea
                    }

                Backward ->
                    { erasedVal = introducedVal
                    , erasedScrollVal = introducedScrollVal
                    , erasedEnv = introducedEnv
                    , erasedArea = introducedArea
                    }

        isPositive : Context -> Bool
        isPositive ctx =
            let
                pol =
                    case goal.execMode of
                        Forward ->
                            Pos

                        Backward ->
                            Neg
            in
            ctx.polarity == pol

        isNegative =
            not << isPositive
    in
    case action of
        Open { ctx } ->
            if erasedArea ctx then
                Err (Erased ctx.zipper)

            else
                Ok ()

        Close { ctx, scroll } ->
            if erasedScrollVal ctx scroll then
                Err (Erased ctx.zipper)

            else if boundary goal.execMode (invert ctx.polarity) scroll.data.outloop /= [] then
                Err (NonEmptyOutloop ctx.zipper)

            else
                case getSingleInloop goal.execMode ctx scroll of
                    Just _ ->
                        Ok ()

                    Nothing ->
                        Err (NonSingleInloop ctx.zipper)

        InsertVal { ctx } ->
            if isPositive ctx then
                Err (InvalidPolarity Pos)

            else if erasedArea ctx then
                Err (Erased ctx.zipper)

            else
                Ok ()

        InsertInloop { ctx, scroll, id } ->
            if isNegative ctx then
                Err (InvalidPolarity Pos)

            else if erasedScrollVal ctx scroll then
                Err (Erased ctx.zipper)

            else if Dict.member id scroll.data.inloops then
                Err (InvalidBranchId ctx.zipper id)

            else
                Ok ()

        DeleteVal { ctx, val } ->
            if isNegative ctx then
                Err (InvalidPolarity Neg)

            else if erasedVal ctx val then
                Err (Erased ctx.zipper)

            else
                Ok ()

        DeleteEnv { ctx, scroll, id } ->
            case Dict.get id scroll.data.inloops of
                Nothing ->
                    Err (InvalidBranchId ctx.zipper id)

                Just env ->
                    if isPositive ctx then
                        Err (InvalidPolarity Neg)

                    else if erasedEnv ctx scroll id env then
                        Err (Erased ctx.zipper)

                    else
                        Ok ()

        IterateVal { srcCtx, srcVal, tgtCtx } ->
            if isNegative tgtCtx then
                Err (InvalidPolarity Neg)

            else if not (spans srcCtx.zipper tgtCtx.zipper) then
                Err (OutOfScope srcCtx.zipper tgtCtx.zipper)

            else if erasedVal srcCtx srcVal then
                Err (Erased srcCtx.zipper)

            else if erasedArea tgtCtx then
                Err (Erased tgtCtx.zipper)

            else if Utils.Maybe.isNothing srcVal.name then
                Err (UnnamedSource srcCtx.zipper)

            else
                Ok ()

        IterateEnv { ctx, scroll, srcId, tgtId } ->
            case Dict.get srcId scroll.data.inloops of
                Nothing ->
                    Err (InvalidBranchId ctx.zipper srcId)

                Just srcEnv ->
                    if isPositive ctx then
                        Err (InvalidPolarity Pos)

                    else if srcId == tgtId then
                        Err (InvalidBranchId ctx.zipper tgtId)

                    else if erasedEnv ctx scroll srcId srcEnv then
                        let
                            zScrollData =
                                { metadata = scroll.metadata
                                , name = scroll.name
                                , justif = scroll.justif
                                , interaction = scroll.data.interaction
                                }

                            zInloop =
                                ZInloop
                                    { scroll = zScrollData
                                    , outloop = scroll.data.outloop
                                    , neighbors = Dict.remove srcId scroll.data.inloops
                                    , metadata = srcEnv.metadata
                                    , justif = srcEnv.justif
                                    , id = srcId
                                    }
                        in
                        Err (Erased (zInloop :: ctx.zipper))

                    else
                        Ok ()

        DeiterateVal { srcCtx, srcVal, tgtCtx, tgtVal } ->
            if isPositive tgtCtx then
                Err (InvalidPolarity Pos)

            else if not (spans srcCtx.zipper tgtCtx.zipper) then
                Err (OutOfScope srcCtx.zipper tgtCtx.zipper)

            else if erasedVal srcCtx srcVal then
                Err (Erased srcCtx.zipper)

            else if erasedVal tgtCtx tgtVal then
                Err (Erased tgtCtx.zipper)

            else if
                boundary goal.execMode srcCtx.polarity [ srcVal ]
                    /= boundary goal.execMode tgtCtx.polarity [ tgtVal ]
            then
                Err (IncompatibleBoundaries srcCtx.zipper tgtCtx.zipper)

            else
                Ok ()

        DeiterateEnv { ctx, scroll, srcId, tgtId } ->
            case Dict.get srcId scroll.data.inloops of
                Nothing ->
                    Err (InvalidBranchId ctx.zipper srcId)

                Just srcEnv ->
                    case Dict.get tgtId scroll.data.inloops of
                        Nothing ->
                            Err (InvalidBranchId ctx.zipper tgtId)

                        Just tgtEnv ->
                            let
                                zScrollData =
                                    { metadata = scroll.metadata
                                    , name = scroll.name
                                    , justif = scroll.justif
                                    , interaction = scroll.data.interaction
                                    }

                                srcZInloop =
                                    ZInloop
                                        { scroll = zScrollData
                                        , outloop = scroll.data.outloop
                                        , neighbors = Dict.remove srcId scroll.data.inloops
                                        , metadata = srcEnv.metadata
                                        , justif = srcEnv.justif
                                        , id = srcId
                                        }

                                tgtZInloop =
                                    ZInloop
                                        { scroll = zScrollData
                                        , outloop = scroll.data.outloop
                                        , neighbors = Dict.remove tgtId scroll.data.inloops
                                        , metadata = tgtEnv.metadata
                                        , justif = tgtEnv.justif
                                        , id = tgtId
                                        }

                                srcZipper =
                                    srcZInloop :: ctx.zipper

                                tgtZipper =
                                    tgtZInloop :: ctx.zipper
                            in
                            if isNegative ctx then
                                Err (InvalidPolarity Pos)

                            else if erasedEnv ctx scroll srcId srcEnv then
                                Err (Erased srcZipper)

                            else if erasedEnv ctx scroll tgtId tgtEnv then
                                Err (Erased tgtZipper)

                            else if
                                boundary goal.execMode ctx.polarity srcEnv.content
                                    /= boundary goal.execMode ctx.polarity tgtEnv.content
                            then
                                Err (IncompatibleBoundaries srcZipper tgtZipper)

                            else
                                Ok ()

        Decompose _ ->
            Ok ()



{- `record action goal` records `action` in `goal` by:
   - generating a new ID `id` and associating `action` to `id` in `goal.actions`
   - pushing `id` in `goal.actionsQueue`
   - decorating the scroll net `goal.net` with the argumentation/interaction corresponding to `action`
   - returning `id` for later usage (typically with `Program.execute`)

   This assumes that the action is indeed applicable in the goal.

   **Note:** for now we assume that `goal.net` is always the top-level net, and thus the action's
   paths are walked from the root of `goal.net`.
-}


record : Action -> Program -> ( Int, Program )
record action goal =
    let
        ( actionId, newActions ) =
            Iddict.insert action goal.actions

        newActionsQueue =
            Queue.enqueue actionId goal.actionsQueue

        newFocus : Net
        newFocus =
            case action of
                Open { ctx, name } ->
                    let
                        inloopId =
                            "Return"

                        interaction =
                            case goal.execMode of
                                Forward ->
                                    { opened = Just inloopId, closed = Nothing }

                                Backward ->
                                    { opened = Nothing, closed = Just inloopId }

                        emptyEnv =
                            { metadata = { grown = False }
                            , justif = assumption
                            , content = []
                            }

                        emptyScroll : Val
                        emptyScroll =
                            { metadata = { grown = False }
                            , name = name
                            , justif = assumption
                            , shape =
                                Scroll
                                    { interaction = interaction
                                    , outloop = []
                                    , inloops = Dict.fromList [ ( inloopId, emptyEnv ) ]
                                    }
                            }
                    in
                    fillZipper [ emptyScroll ] ctx.zipper

                Close { ctx, scroll, id } ->
                    let
                        scrollData =
                            scroll.data

                        interaction =
                            scrollData.interaction

                        newInteraction =
                            case goal.execMode of
                                Forward ->
                                    { interaction | closed = Just id }

                                Backward ->
                                    { interaction | opened = Just id }

                        newScrollData =
                            { scrollData | interaction = newInteraction }

                        newVal =
                            valFromScroll { scroll | data = newScrollData }
                    in
                    fillZipper [ newVal ] ctx.zipper

                InsertVal { ctx, val } ->
                    let
                        grown =
                            isGrownZipper ctx.zipper
                                || isGrownVal val

                        newVal =
                            { val
                                | metadata = { grown = grown }
                                , justif = { self = not grown, from = Nothing }
                            }
                    in
                    fillZipper [ newVal ] ctx.zipper

                InsertInloop { ctx, scroll, id, content } ->
                    let
                        grown =
                            isGrownZipper ctx.zipper
                                || scroll.metadata.grown

                        newInloop =
                            { metadata = { grown = grown }
                            , justif = { self = not grown, from = Nothing }
                            , content = content
                            }

                        scrollData =
                            scroll.data

                        newInloops =
                            Dict.insert id newInloop scrollData.inloops

                        newVal =
                            valFromScroll { scroll | data = { scrollData | inloops = newInloops } }
                    in
                    fillZipper [ newVal ] ctx.zipper

                DeleteVal { ctx, val } ->
                    let
                        oldJustif =
                            val.justif

                        newVal =
                            { val | justif = { oldJustif | self = True } }
                    in
                    fillZipper [ newVal ] ctx.zipper

                DeleteEnv { ctx, scroll, id, env } ->
                    let
                        oldJustif =
                            env.justif

                        newInloop =
                            { env | justif = { oldJustif | self = True } }

                        newInloops =
                            Dict.insert id newInloop scroll.data.inloops

                        oldScrollData =
                            scroll.data

                        newVal =
                            valFromScroll { scroll | data = { oldScrollData | inloops = newInloops } }
                    in
                    fillZipper [ newVal ] ctx.zipper

                IterateVal { srcCtx, srcVal, tgtCtx, tgtName } ->
                    case boundaryVal goal.execMode srcCtx.polarity srcVal of
                        [ node ] ->
                            let
                                copy =
                                    valFromNode node

                                tgtVal =
                                    { copy
                                        | name = tgtName
                                        , justif = { self = False, from = srcVal.name }
                                    }
                            in
                            fillZipper [ tgtVal ] tgtCtx.zipper

                        _ ->
                            -- should never happen
                            Debug.log "Error: boundary of an iterated value should be a single node! Doing nothing."
                                fillZipper
                                []
                                tgtCtx.zipper

                IterateEnv { ctx, scroll, srcId, srcEnv, tgtId } ->
                    let
                        srcBoundary =
                            boundary goal.execMode ctx.polarity srcEnv.content

                        copy =
                            netFromStruct srcBoundary

                        tgtEnv =
                            { srcEnv
                                | justif = { self = False, from = Just srcId }
                                , content = copy
                            }

                        newInloops =
                            Dict.insert tgtId tgtEnv scroll.data.inloops

                        oldScrollData =
                            scroll.data

                        newScroll =
                            { scroll | data = { oldScrollData | inloops = newInloops } }

                        newVal =
                            valFromScroll newScroll
                    in
                    fillZipper [ newVal ] ctx.zipper

                DeiterateVal { srcVal, tgtCtx, tgtVal } ->
                    let
                        oldJustif =
                            tgtVal.justif

                        newVal =
                            { tgtVal | justif = { oldJustif | from = srcVal.name } }
                    in
                    fillZipper [ newVal ] tgtCtx.zipper

                DeiterateEnv { ctx, scroll, srcId, tgtId, tgtEnv } ->
                    let
                        oldJustif =
                            tgtEnv.justif

                        newTgtEnv =
                            { tgtEnv | justif = { oldJustif | from = Just srcId } }

                        newInloops =
                            Dict.insert tgtId newTgtEnv scroll.data.inloops

                        oldScrollData =
                            scroll.data

                        newScroll =
                            { scroll | data = { oldScrollData | inloops = newInloops } }

                        newVal =
                            valFromScroll newScroll
                    in
                    fillZipper [ newVal ] ctx.zipper

                Decompose { ctx, formula } ->
                    fillZipper (decompose formula) ctx.zipper
    in
    ( actionId
    , { goal
        | actions = newActions
        , actionsQueue = newActionsQueue
        , net = newFocus
      }
    )



{- `execute actionId goal` executes the action with ID `actionId` in `goal` by:
   - deleting the associated entry in `goal.actions`
   - deleting `actionId` from `goal.actionsQueue` if not already done
   - applying the semantics of the action in `goal.net`

   **Note:** for now we assume that `goal.net` is always the top-level net, and thus the action's
   paths are walked from the root of `goal.net`.
-}


execute : Int -> Program -> Program
execute actionId goal =
    case Iddict.get actionId goal.actions of
        Just action ->
            let
                newActions =
                    Iddict.remove actionId goal.actions

                newActionsQueue =
                    Queue.filter (\id -> id /= actionId) goal.actionsQueue

                newFocus : Net
                newFocus =
                    case action of
                        Open { ctx, name } ->
                            Debug.todo "Open action execution not implemented yet."

                        Close { ctx, scroll, id } ->
                            Debug.todo "Close action execution not implemented yet."

                        InsertVal { ctx, val } ->
                            Debug.todo "InsertVal action execution not implemented yet."

                        InsertInloop { ctx, scroll, id, content } ->
                            Debug.todo "InsertEnv action execution not implemented yet."

                        DeleteVal { ctx, val } ->
                            Debug.todo "DeleteVal action execution not implemented yet."

                        DeleteEnv { ctx, scroll, id } ->
                            Debug.todo "DeleteEnv action execution not implemented yet."

                        IterateVal { srcCtx, srcVal, tgtCtx, tgtName } ->
                            Debug.todo "IterateVal action execution not implemented yet."

                        IterateEnv { ctx, scroll, srcId, tgtId } ->
                            Debug.todo "IterateEnv action execution not implemented yet."

                        DeiterateVal { srcCtx, srcVal, tgtCtx, tgtVal } ->
                            Debug.todo "DeiterateVal action execution not implemented yet."

                        DeiterateEnv { ctx, scroll, srcId, tgtId } ->
                            Debug.todo "DeiterateEnv action execution not implemented yet."

                        Decompose { ctx, formula } ->
                            Debug.todo "Decompose action execution not implemented yet."
            in
            { goal
                | actions = newActions
                , actionsQueue = newActionsQueue
                , net = newFocus
            }

        Nothing ->
            Debug.log
                "Error: trying to execute action with non-existing ID. Returning the goal unchanged."
                goal


apply : Action -> Program -> Program
apply action goal =
    let
        ( actionId, newProgram ) =
            record action goal
    in
    if goal.recording then
        newProgram

    else
        execute actionId goal



-- A Sandbox is a Program that can be reset


type alias Sandbox =
    { initialProgram : Program
    , currentProgram : Program
    }


type alias Sandboxes =
    Dict SandboxID Sandbox


mkSandbox : Program -> Sandbox
mkSandbox goal =
    { initialProgram = goal
    , currentProgram = goal
    }


getSandbox : SandboxID -> Sandboxes -> Sandbox
getSandbox id sandboxes =
    case Dict.get id sandboxes of
        Nothing ->
            let
                _ =
                    Debug.log "Warning" "trying to retrieve a non-existing sandbox; returning a dummy one."
            in
            mkSandbox (fromNet [])

        Just sandbox ->
            sandbox


updateSandbox : SandboxID -> Program -> Sandboxes -> Sandboxes
updateSandbox id goal sandboxes =
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
                    , currentProgram = goal
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
            [ ( "Flower", ProofMode Interacting, [ s [ a "a", a "b" ] [ [ a "c" ], [ a "d" ] ] ] )
            , ( "QED", ProofMode Interacting, [ s [ a "a" ] [ [] ] ] )
            , ( "Justify", ProofMode Interacting, [ Scroll.identity ] )
            , ( "Unlock", ProofMode Interacting, [ s [ s [] [ [ a "a" ] ] ] [ [ a "a" ] ] ] )
            , ( "Import", ProofMode Interacting, [ Scroll.modusPonensCurryfied ] )
            , ( "Case"
              , ProofMode Interacting
              , [ s [ s [] [ [ a "a" ], [ a "b" ] ], s [ a "a" ] [ [ a "c" ] ], s [ a "b" ] [ [ a "c" ] ] ] [ [ a "c" ] ] ]
              )
            , ( "Decompose", ProofMode Interacting, [ Scroll.orElim ] )
            ]
    in
    examples
        |> List.map (\( id, mode, net ) -> ( id, makeSandbox id mode Forward net ))
        |> Dict.fromList
