module Update.Rules exposing (..)

import Utils.List exposing (forall, zipperFoldl)

import Model.Formula exposing (..)
import Model.Scroll exposing (..)
import Model.Goal exposing (..)


type Rule
  = Decompose -- introduction of connective
  | Justify -- down pollination
  | Import -- up pollination
  | Unlock -- empty outloop with single inloop
  | Case -- empty outloop with many inloops
  | Close -- empty inloop / empty outloop with no inloop
  | Fence -- fencing
  | Reorder -- multiset
  | Grow -- add flower/inloop
  | Crop -- remove flower
  | Pull -- remove inloop


decomposable : Zipper -> Net -> Bool
decomposable _ net =
  case net of
    [Formula { statement }] ->
      case statement of
        Atom _ -> False
        _ -> True
    _ -> False


justifiable : Zipper -> Net -> Bool
justifiable zipper net =
  let
    justifiableFlower flower =
      case flower of
        Formula { statement } ->
          case statement of
            Atom _ -> isHypothesis flower zipper
            _ -> False
        _ ->
          False
  in
  not (List.isEmpty net) &&
  forall justifiableFlower net


unlockable : Zipper -> Net -> Bool
unlockable zipper net =
  case (net, zipper) of
    ([], Outloop { inloops } :: _) ->
      List.length inloops == 1
    _ ->
      False


caseable : Zipper -> Net -> Bool
caseable zipper net =
  case (net, zipper) of
    ([], Outloop { inloops } :: ZNet _ :: Outloop _ :: _) ->
      List.length inloops > 1
    _ ->
      False


closeable : Zipper -> Net -> Bool
closeable zipper net =
  case (net, zipper) of
    ([], Outloop { inloops } :: ZNet _ :: Outloop _ :: _ ) ->
      case inloops of
        [] -> True
        _ -> False
    ([], Inloop _ :: _) -> True
    _ -> False


autoRules : Zipper -> Net -> List Rule
autoRules zipper net =
  let
    rulePreds =
      [ (Decompose, decomposable)
      , (Justify, justifiable)
      , (Unlock, unlockable)
      , (Case, caseable)
      , (Close, closeable) ]
  in
  List.foldl
    (\(rule, pred) acc ->
      if pred zipper net then rule :: acc else acc)
    [] rulePreds


operable : Polarity -> Context -> Bool
operable polarity context =
  context.polarity == polarity ||
  isGrownZipper context.zipper


growable : Context -> Bool
growable =
  operable Pos


glueable : Context -> Bool
glueable =
  operable Neg


pullable : Context -> Bool
pullable =
  operable Pos


croppable : Context -> Bool
croppable =
  operable Neg


operate : Rule -> Zipper -> Net -> Surgery -> Surgery
operate rule zipper net surgery =
  case (rule, net, zipper) of
    (Crop, [flower], ZNet _ :: _) ->
      { surgery | cropped = Just flower }

    (Pull, _, Inloop _ :: _) ->
      { surgery | pulled = Just (mkRealNet net) }
    
    _ ->
      surgery


apply : Rule -> Zipper -> Net -> Net
apply rule zipper net =
  case (rule, net, zipper) of
    (Decompose, [Formula { statement }], _) ->
      fillZipper (decompose statement) zipper

    (Justify, _, _) ->
      fillZipper [] zipper
    
    (Import, _, _) ->
      fillZipper net zipper

    (Unlock, [], Outloop { inloops } :: parent)  ->
      case inloops of
        [inloop] ->
          fillZipper inloop parent
        _ ->
          Debug.todo "Unsupported action"
    
    (Case, [], Outloop branches
            :: ZNet { left, right }
            :: Outloop goal :: parent) ->
      let
        case_ : Net -> Flower
        case_ branch =
          mkFlower branches.metadata branch goal.inloops
        
        outloop =
          mkNet goal.outloopMetadata (left ++ right)
        
        cases =
          List.map case_ branches.inloops
      in
      fillZipper [mkFlower goal.metadata outloop [mkRealNet cases]] parent
    
    (Close, [], Outloop p :: ZNet _ :: Outloop _ :: parent ) ->
      if List.isEmpty p.inloops then
        fillZipper [] parent
      else
        Debug.todo "Unsupported action"
    
    (Close, [], Inloop _ :: parent) ->
      fillZipper [] parent
    
    (Reorder, _, _ :: parent) ->
      fillZipper net parent
    
    (Grow, _, _) ->
      fillZipper net zipper
    
    (Crop, _, _) ->
      fillZipper [] zipper

    (Pull, _, Inloop { metadata, outloop, left, right } :: parent) ->
      fillZipper [mkFlower metadata outloop (left ++ right)] parent

    _ ->
      Debug.todo "Unsupported action"


tryRules : List Rule -> List Rule -> Zipper -> Net -> Maybe Net
tryRules candidates allowed zipper net =
  case candidates of
    [] ->
      Nothing
    rule :: rules ->
      if not (List.member rule allowed) then
        tryRules rules allowed zipper net
      else
        Just (apply rule zipper net)


autoFlower : List Rule -> Zipper -> Flower -> Maybe Net
autoFlower allowed zipper flower =
  case flower of
    Formula _ ->
      let candidates = autoRules zipper [flower] in
      tryRules candidates allowed zipper [flower]
    
    Flower { metadata, outloop, inloops } ->
      -- First try on outloop
      let (ZNet outloopData) = outloop in
      let resultOutloop = autoNet allowed (mkOutloop metadata outloopData.metadata inloops :: zipper) outloop in
      case resultOutloop of
        Just _ -> resultOutloop
        Nothing ->
          -- Then try on inloops
          let
            autoInloop (left, right) (ZNet inloopData as inloop) acc =
              case acc of
                Just _ -> acc
                Nothing ->
                  autoNet allowed (mkInloop metadata inloopData.metadata outloop left right :: zipper) inloop
          in
          zipperFoldl autoInloop Nothing inloops


autoNet : List Rule -> Zipper -> Net -> Maybe Net
autoNet allowed zipper net =
  autoNet allowed zipper net


autoNet : List Rule -> Zipper -> Net -> Maybe Net
autoNet allowed zipper net =
  -- First try on the whole net
  let
    candidates = autoRules zipper net
    wholeResult = tryRules candidates allowed zipper net
  in
  case wholeResult of
    Just _ -> wholeResult
    Nothing ->
      -- Then try on each flower of the net
      let
        -- Let's assume flowers in a net are always tulips, shall we?
        autoTulip (left, right) tulip acc =
          case acc of
            Just _ -> acc
            Nothing ->
              autoFlower allowed (mkNet left right :: zipper) tulip
      in
      zipperFoldl autoTulip Nothing net


auto : List Rule -> Net -> Net
auto allowed net =
  case autoNet allowed [] net of
    Just result ->
      auto allowed result
    Nothing ->
      net