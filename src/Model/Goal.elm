module Model.Goal exposing (..)

import Model.Scroll as Scroll exposing (..)

import Dict exposing (Dict)
import Queue exposing (Queue)
import Iddict exposing (Iddict)

import Utils.Maybe
import Browser exposing (application)
import Css exposing (invalid)


-- Selection


type alias Selection
  = List Context


-- Modal UI


type ProofInteraction
  = Interacting
  | Argumenting


{- A `Surgery` remembers the last deleted value or environment in Edit mode,
   so that it can be pasted elsewhere.
-}
type alias Surgery =
  { cropped : Maybe Val
  , pulled : Maybe Env }


initialSurgery : Surgery
initialSurgery =
  { cropped = Nothing
  , pulled = Nothing }


type EditInteraction
  = Operating
  | Adding Context
  | Renaming Context
  | Reordering


type ActionMode
  = ProofMode ProofInteraction
  | EditMode EditInteraction Surgery
  | NavigationMode


type ExecMode
  = Forward
  | Backward


-- Navigation
  

{- A `Navigation` is a non-empty sequence of visited [Context]s.

   It is used to keep track of the user's navigation history, i.e. the different
   locations in a goal that she has visited.

   The head of the list corresponds to the last visited location, and the last
   element to the initial location.
-}
type alias Navigation = List Context


initialNavigation : Navigation
initialNavigation =
  [Context [] Pos]


visit : Context -> Navigation -> Navigation
visit context navigation =
  context :: navigation


current : Navigation -> Context
current navigation =
  case navigation of
    context :: _ ->
      context

    [] ->
      Context [] Pos -- should never happen


changeFocus : Context -> Navigation -> Navigation
changeFocus context navigation =
  case navigation of
    _ :: rest ->
      context :: rest

    [] ->
      [context] -- should never happen


backtrack : Navigation -> Navigation
backtrack navigation =
  case navigation of
    [_] ->
      navigation
      
    _ :: (_ :: _ as rest) ->
      rest

    [] ->
      initialNavigation -- should never happen


-- Goals


type alias SandboxID = String


type Location
  = App
  | Manual SandboxID

{- A `Goal` is made of the following data:

   `focus`: the net that the user is currently working on

   `navigation`: the navigation history

   `location`: a unique, semantic identifier for the goal's location in the app

   `actionMode`: the current mode determining which actions can be performed through direct manipulation

   `execMode`: the current mode determining in which direction actions are executed

   `recording`: a boolean determining whether actions are recorded
   
   `actions`: dictionary of all recorded actions pending for execution, accessed through unique IDs

   `actionsQueue`: queue of recorded actions IDs
-}
type alias Goal
  = { focus : Net
    , navigation : Navigation
    , location : Location
    , actionMode : ActionMode
    , execMode : ExecMode
    , recording : Bool
    , actions : Iddict Action
    , actionsQueue : Queue Int
    }


fromNet : Net -> Goal
fromNet net =
  { focus = net
  , navigation = initialNavigation
  , location = App
  , actionMode = ProofMode Interacting
  , execMode = Forward
  , recording = True
  , actions = Iddict.empty
  , actionsQueue = Queue.empty
  }


map : (Net -> Net) -> Goal -> Goal
map f goal =
  { goal | focus = f goal.focus }


-- Actions


type Action
  -- Open a scroll with an empty outloop and a single empty inloop
  = Open { ctx : Context } 
  -- Close a scroll with an empty outloop and a single inloop
  | Close { ctx : Context, scroll : ScrollVal } 
  -- Insert a value in a net
  | InsertVal { ctx : Context, val : Val }
  -- Insert an inloop in a scroll
  | InsertEnv { ctx : Context, scroll : ScrollVal, id : Ident, env : Env }
  -- Delete a value from a net
  | DeleteVal { ctx : Context, val : Val }
  -- Delete an inloop from a scroll
  | DeleteEnv { ctx : Context, scroll : ScrollVal, id : Ident } 
  -- Iterate a source value into a net
  | IterateVal { srcCtx : Context, srcVal : Val, tgtCtx : Context, tgtName : Maybe Ident } 
  -- Iterate a source inloop in the same scroll
  | IterateEnv { ctx : Context, scroll : ScrollVal, id : Ident, copyId : Ident } 
  -- Deiterate a target value from an identical source
  | DeiterateVal { srcCtx : Context, srcVal : Val, tgtCtx : Context, tgtVal : Val } 
  -- Deiterate a target inloop from an identical source in the same scroll
  | DeiterateEnv { ctx : Context, scroll : ScrollVal, srcId : Ident, tgtId : Ident } 


type ActionError
  = InvalidPolarity Polarity
  | InvalidBranchId Zipper Ident
  | Erased Zipper
  | NonEmptyOutloop Zipper
  | OutOfScope Zipper Zipper
  | IncompatibleBoundaries Zipper Zipper


boundary : ExecMode -> Polarity -> Net -> Struct
boundary execMode =
  let
    forwardBoundary pol =
      case pol of
        Pos -> conclusion
        Neg -> premiss
  in
  case execMode of
    Forward -> forwardBoundary
    Backward -> invert >> forwardBoundary


applicable : Goal -> Action -> Result ActionError ()
applicable goal action =
  let
    (erasedVal, erasedScrollVal, erasedEnv, erasedArea) =
      
      case goal.execMode of
        Forward ->
          ( eliminatedVal
          , eliminatedScrollVal
          , eliminatedEnv
          , eliminatedArea )
        Backward ->
          ( introducedVal
          , introducedScrollVal
          , introducedEnv
          , introducedArea )

    isPositive : Context -> Bool
    isPositive ctx =
      let
        pol =
          case goal.execMode of
            Forward -> Pos
            Backward -> Neg
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
        Ok ()

    InsertVal { ctx } ->
      if isPositive ctx then
        Err (InvalidPolarity Pos)
      else if erasedArea ctx then
        Err (Erased ctx.zipper)
      else
        Ok ()

    InsertEnv { ctx, scroll, id } ->
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
      else
        Ok ()
    
    IterateEnv { ctx, scroll, id, copyId } ->
      case Dict.get id scroll.data.inloops of
        Nothing ->
          Err (InvalidBranchId ctx.zipper id)
        Just env ->
          if isPositive ctx then
            Err (InvalidPolarity Pos)
          else if id == copyId then
            Err (InvalidBranchId ctx.zipper copyId)
          else if erasedEnv ctx scroll id env then
            let
              zScrollData =
                { metadata = scroll.metadata
                , name = scroll.name
                , justif = scroll.justif
                , interaction = scroll.data.interaction }
              zInloop =
                ZInloop { scroll = zScrollData
                        , outloop = scroll.data.outloop
                        , neighbors = Dict.remove id scroll.data.inloops
                        , metadata = env.metadata
                        , justif = env.justif
                        , id = id }
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
      else if boundary goal.execMode srcCtx.polarity [srcVal] /=
              boundary goal.execMode tgtCtx.polarity [tgtVal] then
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
                  , interaction = scroll.data.interaction }
                srcZInloop =
                  ZInloop { scroll = zScrollData
                          , outloop = scroll.data.outloop
                          , neighbors = Dict.remove srcId scroll.data.inloops
                          , metadata = srcEnv.metadata
                          , justif = srcEnv.justif
                          , id = srcId }
                tgtZInloop =
                  ZInloop { scroll = zScrollData
                          , outloop = scroll.data.outloop
                          , neighbors = Dict.remove tgtId scroll.data.inloops
                          , metadata = tgtEnv.metadata
                          , justif = tgtEnv.justif
                          , id = tgtId }
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
              else if boundary goal.execMode ctx.polarity srcEnv.content /=
                      boundary goal.execMode ctx.polarity srcEnv.content then
                Err (IncompatibleBoundaries srcZipper tgtZipper)
              else
                Ok ()


{- `record action goal` records `action` in `goal` by:
   - generating a new ID `id` and associating `action` to `id` in `goal.actions`
   - pushing `id` in `goal.actionsQueue`
   - decorating the scroll net `goal.focus` with the argumentation/interaction corresponding to `action`
   - returning `id` for later usage (typically with `Goal.execute`)
   
   This assumes that the action is indeed applicable in the goal.

   **Note:** for now we assume that `goal.focus` is always the top-level net, and thus the action's
   paths are walked from the root of `goal.focus`.
-}
record : Action -> Goal -> (Int, Goal)
record action goal =
  let
    (id, newActions) = Iddict.insert action goal.actions
    newActionsQueue = Queue.enqueue id goal.actionsQueue
    freshName = "x" ++ String.fromInt id

    newFocus : Net
    newFocus =
      let
        walk path =
          let ({ zipper }, net) = walkGoal goal path in
          (zipper, net)

        skip () =
          Debug.log
            "Unexpected action path. Doing nothing."
            goal.focus
      in
      case action of
        Open path ->
          case walk path of
            (zipper, net) ->
              let
                interaction =
                  case goal.execMode of
                    Forward ->
                      { opened = Just 0, closed = Nothing }
                    Backward ->
                      { opened = Nothing, closed = Just 0 }
                  
                emptyScroll =
                  { metadata = { grown = False }
                  , arg = { name = Just freshName, justif = assumption }
                  , shape = Scroll { interaction = interaction
                                    , outloop = []
                                    , inloops = [mkEnv { grown = False } Nothing []] } }
                
                newNet =
                  net ++ [emptyScroll]
              in
              fillZipper newNet zipper

        Close path ->
          case walk path of
            (ZNet { left } :: _ as zipper, [val]) ->
              case val.shape of
                Scroll ({ interaction } as scrollData) ->
                  let
                    idx = List.length left
                    newInteraction =
                      case goal.execMode of
                        Forward ->
                          { interaction | closed = Just idx }
                        Backward ->
                          { interaction | opened = Just idx }
                    newScrollData =
                      { scrollData | interaction = newInteraction }
                  in
                  fillZipper [{ val | shape = Scroll newScrollData }] zipper
                
                _ -> skip ()
            _ -> skip ()

        Insert path content ->
          case (walk path, content) of
            -- Insert new inloop
            ((ZNet _ :: _ as zipper, [val]), inloopContent) ->
              case val.shape of
                Scroll scroll ->
                  let
                    selfJustified =
                      not (isGrownZipper zipper || isGrownVal val)
                    
                    newInloop =
                      { metadata = { grown = selfJustified }
                      , arg = { name = Just freshName
                              , justif = { self = selfJustified, from = Nothing } }
                      , content = inloopContent }

                    newScroll =
                      Scroll { scroll | inloops = scroll.inloops ++ [newInloop] }
                    
                    newVal =
                      { val | shape = newScroll }
                  in
                  fillZipper [newVal] zipper
                
                _ -> skip ()

            -- Insert new value
            ((zipper, net), [val]) ->
              let
                selfJustified =
                  not (isGrownZipper zipper || isGrownVal val)
                
                newVal =
                  { val | metadata = { grown = selfJustified }
                        , arg = { name = Just freshName
                                , justif = { self = selfJustified, from = Nothing } } }
              in
              fillZipper (net ++ [newVal]) zipper
            
            _ -> skip ()

        Delete path ->
          case walk path of
            -- Delete inloop
            (ZInloop { scroll, metadata, arg, outloop, left, right } :: zipper, inloopContent) ->
              let
                oldJustif = arg.justif

                newInloop =
                  { metadata = metadata
                  , arg = { arg | justif = { oldJustif | self = True } }
                  , content = inloopContent }

                newVal =
                  { metadata = scroll.metadata
                  , arg = scroll.arg
                  , shape = Scroll { interaction = scroll.interaction
                                   , outloop = outloop
                                   , inloops = left ++ newInloop :: right } }
              in
              fillZipper [newVal] zipper
            
            -- Delete value
            (zipper, [val]) ->
              let
                oldJustif = val.arg.justif
                oldArg = val.arg
                newVal = { val | arg = { oldArg | justif = { oldJustif | self = True } } }
              in
              fillZipper [newVal] zipper
            
            _ -> skip ()

        Iterate { src, tgt } ->
          case (walk src, walk tgt) of
            -- Iterate an inloop
            ((ZInloop zinloop :: _, inloopContent), (ZNet _ :: _ as zipper, [val])) ->
              case val.shape of
                Scroll tgtScroll ->
                  let
                    copyInloop =
                      { metadata = { grown = False }
                      , arg = { name = Just freshName
                              , justif = { self = False, from = zinloop.arg.name } }
                      , content = inloopContent }
                      
                    newScroll =
                      Scroll { tgtScroll | inloops = tgtScroll.inloops ++ [copyInloop] }

                    newVal =
                      { val | shape = newScroll }
                  in
                  fillZipper [newVal] zipper
              
                _ -> skip ()

            -- Iterate a value
            ((_, [val]), (zipper, net)) ->
              let
                copyVal =
                  { metadata = { grown = False }
                  , arg = { name = Just freshName
                          , justif = { self = False, from = val.arg.name } }
                  , shape = val.shape }
              in
              fillZipper (net ++ [copyVal]) zipper
            
            _ -> skip ()

        Deiterate { src, tgt } ->
          case (walk src, walk tgt) of
            -- Deiterate an inloop
            ((ZInloop srcZInloop :: _, _), (ZInloop tgtZInloop :: zipper, tgtInloopContent)) ->
              let
                oldArg = tgtZInloop.arg
                oldJustif = oldArg.justif
                newJustif = { oldJustif | from = srcZInloop.arg.name }
                newZInloop =
                  ZInloop { tgtZInloop | arg = { oldArg | justif = newJustif } }
              in
              fillZipper tgtInloopContent (newZInloop :: zipper)

            -- Deiterate a value
            ((_, [srcVal]), (zipper, [tgtVal])) ->
              let
                oldArg = tgtVal.arg
                oldJustif = oldArg.justif
                newJustif = { oldJustif | from = srcVal.arg.name }
                newVal =
                  { tgtVal | arg = { oldArg | justif = newJustif } }
              in
              fillZipper [newVal] zipper

            _ -> skip ()
  in
  (id, { goal | actions = newActions
              , actionsQueue = newActionsQueue
              , focus = newFocus })


{- `execute actionId goal` executes the action with ID `actionId` in `goal` by:
   - deleting the associated entry in `goal.actions`
   - deleting `actionId` from `goal.actionsQueue` if not already done
   - applying the semantics of the action in `goal.focus`

   **Note:** for now we assume that `goal.focus` is always the top-level net, and thus the action's
   paths are walked from the root of `goal.focus`.
-}
execute : Int -> Goal -> Goal
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
          let
            walk path =
              let ({ zipper }, net) = walkGoal goal path in
              (zipper, net)
          in
          case action of
            Open path ->
              Debug.todo "Open action execution not implemented yet."

            Close path ->
              Debug.todo "Close action execution not implemented yet."

            Insert path content ->
              Debug.todo "Insert action execution not implemented yet."

            Delete path ->
              case walk path of
                -- Delete inloop
                (ZInloop zinloop :: zipper, _) ->
                  let
                    newScroll =
                      { metadata = zinloop.scroll.metadata
                      , arg = zinloop.scroll.arg
                      , shape = Scroll { interaction = zinloop.scroll.interaction
                                       , outloop = zinloop.outloop
                                       , inloops = zinloop.left ++ zinloop.right } }
                  in
                  fillZipper [newScroll] zipper
                
                -- Delete value
                (zipper, _) ->
                  fillZipper [] zipper

            Iterate { src, tgt } ->
              Debug.todo "Iterate action execution not implemented yet."

            Deiterate { src, tgt } ->
              Debug.todo "Deiterate action execution not implemented yet."
      in
      { goal | actions = newActions
             , actionsQueue = newActionsQueue
             , focus = newFocus }

    Nothing ->
      Debug.log
        "Error: trying to execute action with non-existing ID. Returning the goal unchanged."
        goal


-- A Sandbox is a Goal that can be reset


type alias Sandbox =
  { initialGoal : Goal
  , currentGoal : Goal
  }

type alias Sandboxes = Dict SandboxID Sandbox


mkSandbox : Goal -> Sandbox
mkSandbox goal =
  { initialGoal = goal
  , currentGoal = goal
  }


getSandbox : SandboxID -> Sandboxes -> Sandbox
getSandbox id sandboxes =
  case Dict.get id sandboxes of
    Nothing ->
      let _ = Debug.log "Warning" "trying to retrieve a non-existing sandbox; returning a dummy one." in
      mkSandbox (fromNet [])

    Just sandbox ->
      sandbox


updateSandbox : SandboxID -> Goal -> Sandboxes -> Sandboxes
updateSandbox id goal sandboxes =
  case Dict.get id sandboxes of
    Nothing ->
      let _ = Debug.log "Warning" "trying to update a non-existing sandbox. Ignoring." in
      sandboxes

    Just sandbox ->
      let
        updatedSandbox =
          { initialGoal = sandbox.initialGoal
          , currentGoal = goal
          }
      in
      Dict.insert id updatedSandbox sandboxes


resetSandbox : SandboxID -> Sandboxes -> Sandboxes
resetSandbox id sandboxes =
  case Dict.get id sandboxes of
    Nothing ->
      let _ = Debug.log "Warning" "trying to reset a non-existing sandbox. Ignoring." in
      sandboxes

    Just sandbox ->
      updateSandbox id sandbox.initialGoal sandboxes


resetAllSandboxes : Sandboxes -> Sandboxes
resetAllSandboxes sandboxes =
  Dict.map (\_ sb -> { sb | currentGoal = sb.initialGoal }) sandboxes


manualExamples : Sandboxes
manualExamples =
  let
    makeSandbox id actionMode execMode focus =
      mkSandbox
        { focus = focus
        , navigation = initialNavigation
        , location = Manual id
        , actionMode = actionMode
        , execMode = execMode
        , recording = True
        , actions = Iddict.empty
        , actionsQueue = Queue.empty
        }
    
    examples : List (SandboxID, ActionMode, Net)
    examples =
      [ ( "Flower", ProofMode Interacting, [s[a"a",a"b"][[a"c"],[a"d"]]] )
      , ( "QED", ProofMode Interacting, [s[a"a"][[]]] )
      , ( "Justify", ProofMode Interacting, [Scroll.identity] )
      , ( "Unlock", ProofMode Interacting, [s[s[][[a"a"]]][[a"a"]]] )
      , ( "Import", ProofMode Interacting, [Scroll.modusPonensCurryfied] )
      , ( "Case", ProofMode Interacting,
          [s[s[][[a"a"],[a"b"]],s[a"a"][[a"c"]],s[a"b"][[a"c"]]][[a"c"]]] )
      , ( "Decompose", ProofMode Interacting, [Scroll.orElim] )
      ]
  in
  examples |>
  List.map (\(id, mode, net) -> (id, makeSandbox id mode Forward net)) |>
  Dict.fromList