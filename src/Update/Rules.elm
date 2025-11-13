module Update.Rules exposing (..)

import Utils.List exposing (forall, zipperFoldl)

import Model.Formula exposing (..)
import Model.Scroll exposing (..)
import Model.Goal exposing (..)


type Rule
  = Open -- open scroll with empty outloop and single empty inloop
  | Close -- close scroll with empty outloop and single (possibly non-empty) inloop
  | Insert -- insert a judgment/inloop
  | Delete -- delete a judgment/inloop
  | Iterate -- iterate a judgment/inloop
  | Deiterate -- deiterate a judgment/inloop
  | Decompose -- decomposition of connective
  | Reorder -- reorder judgments in a net


decomposable : Zipper -> Net -> Bool
decomposable _ net =
  case net of
    [{ shape }] ->
      case shape of
        Formula formula ->
          case formula of
            Atom _ -> False
            _ -> True
        _ -> False
    _ ->
      False


justifiable : Zipper -> Net -> Bool
justifiable zipper net =
  let
    justifiableJudgment judgment =
      case judgment.shape of
        Formula formula ->
          case formula of
            Atom _ -> isHypothesis judgment zipper
            _ -> False
        _ ->
          False
  in
  not (List.isEmpty net) &&
  forall justifiableJudgment net


closeable : Zipper -> Net -> Bool
closeable zipper net =
  case (net, zipper) of
    ([], ZOutloop { inloops } :: _) ->
      List.length inloops == 1
    _ ->
      False


autoRules : Zipper -> Net -> List Rule
autoRules zipper net =
  let
    rulePreds =
      [ (Decompose, decomposable)
      , (Deiterate, justifiable)
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
  operable Neg


glueable : Context -> Bool
glueable =
  operable Pos


pullable : Context -> Bool
pullable =
  operable Neg


croppable : Context -> Bool
croppable =
  operable Pos


{- `operate rule zipper net surgery` updates `surgery` with the deleted `net` if `rule` is a
   `Deletion`, and does nothing otherwise.
-}
operate : Rule -> Zipper -> Net -> Surgery -> Surgery
operate rule zipper net surgery =
  case (rule, net, zipper) of
    (Delete, [judgment], ZNet _ :: _) ->
      { surgery | cropped = Just judgment }

    (Delete, _, ZInloop inloop :: _) ->
      { surgery | pulled = Just { ident = inloop.ident, content = net } }
    
    _ ->
      surgery


apply : Rule -> Zipper -> Net -> Result String Net
apply rule zipper net =
  case (rule, net, zipper) of
    (Decompose, [{ shape }], _) ->
      case shape of
        Formula formula ->
          Ok (fillZipper (decompose formula) zipper)
        
        _ ->
          Err "Cannot decompose something else than a formula"

    (Deiterate, _, _) ->
      Ok (fillZipper net zipper)
    
    (Iterate, _, _) ->
      Ok (fillZipper net zipper)

    (Close, [], ZOutloop _ :: _)  ->
      Ok (fillZipper net zipper)
    
    (Reorder, _, _ :: parent) ->
      Ok (fillZipper net parent)
    
    (Insert, _, _) ->
      Ok (fillZipper net zipper)
    
    (Delete, _, _) ->
      Ok (fillZipper [] zipper)

    _ ->
      Err "Unsupported action"


tryRules : List Rule -> List Rule -> Zipper -> Net -> Maybe Net
tryRules candidates allowed zipper net =
  case candidates of
    [] ->
      Nothing
    rule :: rules ->
      if not (List.member rule allowed) then
        tryRules rules allowed zipper net
      else
        case apply rule zipper net of
          Ok res -> Just res
          Err msg -> Debug.log msg Nothing


autoJudgment : List Rule -> Zipper -> Argument -> Maybe Net
autoJudgment allowed zipper judgment =
  case judgment.shape of
    Formula _ ->
      let candidates = autoRules zipper [judgment] in
      tryRules candidates allowed zipper [judgment]
    
    Scroll { interaction, outloop, inloops } ->
      -- First try on outloop
      let 
        scrollData =
          { metadata = judgment.metadata
          , ident = judgment.name
          , justif = judgment.justif
          , interaction = interaction }

        resultOutloop =
          autoNet allowed (mkZOutloop scrollData inloops :: zipper) outloop
      in
      case resultOutloop of
        Just _ -> resultOutloop
        Nothing ->
          -- Then try on inloops
          let
            autoInloop (left, right) { ident, content } acc =
              case acc of
                Just _ -> acc
                Nothing ->
                  autoNet allowed (mkZInloop scrollData ident outloop left right :: zipper) content
          in
          zipperFoldl autoInloop Nothing inloops


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
        aux (left, right) judgment acc =
          case acc of
            Just _ -> acc
            Nothing ->
              autoJudgment allowed (mkZNet left right :: zipper) judgment
      in
      zipperFoldl aux Nothing net


auto : List Rule -> Net -> Net
auto allowed net =
  case autoNet allowed [] net of
    Just result ->
      auto allowed result
    Nothing ->
      net