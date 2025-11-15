module Model.Goal exposing (..)

import Model.Scroll as Scroll exposing (..)

import Dict exposing (Dict)
import Queue exposing (Queue)
import Iddict exposing (Iddict)


-- Selection


type alias Selection
  = List Path


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
  | Adding Path
  | Renaming Path
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


walkGoal : Goal -> Path -> (Context, Net)
walkGoal { focus } path =
  case Scroll.walk focus path of
    Just (ctx, net) ->
      (ctx, net)
    Nothing ->
      Debug.log
        "Invalid path in goal. Returning the empty context."
        ({ zipper = [], polarity = Pos }, focus) -- should not happen


-- Actions


type Action
  = Open Path -- open a scroll with an empty outloop and a single empty inloop at the end of a net
  | Close Path -- close a scroll with an empty outloop
  | Insert Path Net -- insert a value/inloop at the end of a net/scroll
  | Delete Path -- delete a value/inloop from a net/scroll
  | Iterate { src : Path, tgt : Path } -- iterate a source value/inloop at the end of a target net/scroll
  | Deiterate { src : Path, tgt : Path } -- deiterate a target value/inloop from an identical source


{- `record action goal` records `action` in `goal` by:
   - generating a new ID `id` and associating `action` to `id` in `goal.actions`
   - pushing `id` in `goal.actionsQueue`
   - decorating the scroll net `goal.focus` with the argumentation/interaction corresponding to `action`
   - returning `id` for later usage (typically with `Goal.execute`)

   **Note:** for now we assume that `goal.focus` is always the top-level net, and thus the action's
   paths are walked from the root of `goal.focus`.
-}
record : Action -> Goal -> (Int, Goal)
record action goal =
  let
    (id, newActions) = Iddict.insert action goal.actions
    newActionsQueue = Queue.enqueue id goal.actionsQueue

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
                emptyScroll =
                  mkShape { grown = False }
                    (Scroll { interaction = { opened = Just 0, closed = Nothing }
                            , outloop = []
                            , inloops = [mkInloop { grown = False } Nothing []] })
                
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
                    newInteraction =
                      { interaction | closed = Just (List.length left) }
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
                      , arg = { name = Nothing
                              , justif = { self = selfJustified, from = Nothing } }
                      , content = inloopContent }

                    newScroll =
                      Scroll { interaction = scroll.interaction
                             , outloop = scroll.outloop
                             , inloops = scroll.inloops ++ [newInloop] }
                    
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
                        , arg = { name = Nothing
                                , justif = { self = selfJustified, from = Nothing } } }
              in
              fillZipper (net ++ [newVal]) zipper
            
            _ -> skip ()

        Delete path ->
          Debug.todo "Delete action recording not implemented yet."

        Iterate { src, tgt } ->
          Debug.todo "Iterate action recording not implemented yet."

        Deiterate { src, tgt } ->
          Debug.todo "Deiterate action recording not implemented yet."
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

        walk = walkGoal goal

        newFocus : Net
        newFocus =
          case action of
            Open path ->
              Debug.todo "Open action execution not implemented yet."

            Close path ->
              Debug.todo "Close action execution not implemented yet."

            Insert path content ->
              Debug.todo "Insert action execution not implemented yet."

            Delete path ->
              Debug.todo "Delete action execution not implemented yet."

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

type alias SandboxID = String

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