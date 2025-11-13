module Model.Goal exposing (..)

import Model.Scroll as Scroll exposing (..)

import Dict exposing (Dict)
import Queue exposing (Queue)


-- Selection


type alias Selection
  = List Zipper


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
  | Adding Zipper
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

   `location`: a unique, semantic identifier for the goal's location

   `actionMode`: the current mode determining which actions can be performed through direct manipulation

   `execMode`: the current mode determining in which direction actions are executed

   `recording`: a boolean determining whether actions are recorded

   `actionsQueue`: queue of recorded actions pending for execution
-}
type alias Goal
  = { focus : Net
    , navigation : Navigation
    , location : Location
    , actionMode : ActionMode
    , execMode : ExecMode
    , recording : Bool
    , actions : Queue Action
    }


fromNet : Net -> Goal
fromNet net =
  { focus = net
  , navigation = initialNavigation
  , location = App
  , actionMode = ProofMode Interacting
  , execMode = Forward
  , recording = True
  , actions = Queue.empty
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
      ({ zipper = [], polarity = Pos }, focus) -- should not happen

-- Actions


type Action
  = Open Path -- open a scroll with an empty outloop and a single empty inloop at the end of a net
  | Close Path -- close a scroll with an empty outloop
  | Insert Path Net -- insert a value/inloop at the end of a net/scroll
  | Delete Path -- delete a value/inloop from a net/scroll
  | Iterate { src : Path, dst : Path } -- iterate a source value/inloop at the end of a target net/scroll
  | Deiterate { src : Path, dst : Path } -- deiterate a target value/inloop inloop from an identical source


{- `exec action goal` executes `action` in `goal` by updating `goal.focus` accordingly.

   Note: for now we assume that `goal.focus` is always the top-level net, and thus the action's
   paths are walked from the root of `goal.focus`.
-}
exec : Action -> Goal -> Goal
exec action goal =
  let
    walk = walkGoal goal

    newFocus : Net
    newFocus =
      case action of
        Open path ->
          Debug.todo ""

        Close path ->
          Debug.todo ""

        Insert path net ->
          Debug.todo ""

        Delete path ->
          Debug.todo ""

        Iterate { src, dst } ->
          Debug.todo ""

        Deiterate { src, dst } ->
          Debug.todo ""
  in
  { goal | focus = newFocus }


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
        , actions = Queue.empty
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