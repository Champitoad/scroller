module View.Goal exposing (..)

import View.Style as Style exposing (..)
import View.Widgets as Widgets exposing (..)
import View.Events

import Model.Formula as Formula exposing (..)
import Model.Scroll as Scroll exposing (..)
import Model.Mascarpone exposing (..)
import Model.Goal exposing (..)
import Model.App exposing (..)

import Update.App exposing (..)

import Utils.List
import Utils.Events
import Utils.Color

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

import Html.Attributes exposing (title)

import Html5.DragDrop as DnD

import Css

import FeatherIcons as Icons

import Color
import Utils.Color
import Utils.Events exposing (onClick)

import Dict


reorderColor : Color.Color
reorderColor =
  Utils.Color.fromRgb { red = 0.7, green = 0.7, blue = 0.7 }


useColor : Color.Color
useColor =
  Utils.Color.fromRgb { red = 1, green = 0.8, blue = 0 }


viewAction : Goal -> Action -> ZoneStyle Msg -> String -> List (Attribute Msg)
viewAction goal action actionableStyle titleText =
  case applicable goal action of
    Err _ ->
      actionableStyle.inactive
    Ok _ ->
      Utils.Events.onClick (Apply goal.location action) ::
      (htmlAttribute <| title titleText) ::
      actionableStyle.active


deleteValAction : Goal -> Context -> Val -> List (Attribute Msg)
deleteValAction goal ctx val =
  viewAction goal
    (DeleteVal { ctx = ctx, val = val })
    redActionable "Delete value"


deleteEnvAction : Goal -> Context -> ScrollVal -> Scroll.Ident -> Env -> List (Attribute Msg)
deleteEnvAction goal ctx scroll id env =
  viewAction goal
    (DeleteEnv { ctx = ctx, scroll = scroll, id = id, env = env })
    redActionable "Delete branch"


drawGrownBorder : Bool -> List (Attribute msg)
drawGrownBorder doit =
  if doit then grownBorder.active else grownBorder.inactive


viewAtom : Formula.Ident -> Element Msg
viewAtom ident =
  case ident of
    Name name ->
      text name
    
    Image data ->
      image [width (px 100), height (px 100)] data


viewStatement : Formula -> Element Msg
viewStatement formula =
  case formula of
    Atom ident ->
      viewAtom ident
    
    Truth ->
      text "⊤"
    
    Falsity ->
      text "⊥"
    
    And f1 f2 ->
      row [] [viewStatement f1, text " ∧ ", viewStatement f2]

    Or f1 f2 ->
      row [] [viewStatement f1, text " v ", viewStatement f2]

    Implies f1 f2 ->
      row [] [text "(", viewStatement f1, text " ⇒ ", viewStatement f2, text ")"]
    
    Not f1 ->
      row [] [text "¬ (", viewStatement f1, text ")"]


viewFormula : ValDnD -> Goal -> Context -> Metadata -> Maybe Scroll.Ident -> Justification -> Formula -> Element Msg
viewFormula dnd goal ctx metadata name justif formula =
  let
    val =
      { metadata = metadata
      , name = name
      , justif = justif
      , shape = Formula formula }

    clickAction =
      case goal.actionMode of
        ProofMode Interacting ->
          case formula of
            Atom _ -> []
            _ ->
              viewAction goal
                (Decompose { ctx = ctx, formula = formula })
                pinkActionable "Decompose"
        
        EditMode { interaction } ->
          case interaction of
            Operating -> deleteValAction goal ctx val
            _ -> []

        _ -> []
    
    dragAction =
      case goal.actionMode of
        ProofMode Interacting ->
          View.Events.dragAction useColor dnd goal.location ctx val

        EditMode _ ->
          View.Events.dragAction reorderColor dnd goal.location ctx val

        _ -> []
    
    (fontSize, paddingSize) =
      case goal.location of
        App -> (45, 10)
        Manual _ -> (30, 5)
  in
  el
    ( [ centerX, centerY
      , padding paddingSize
      , Font.color (scrollForegroundColor ctx.polarity)
      , Font.size fontSize
      , nonSelectable ]
      ++ dragAction
      ++ clickAction
      ++ drawGrownBorder metadata.grown )
    ( viewStatement formula )


viewOutloop : ValDnD -> Goal -> Context -> ScrollVal -> Element Msg
viewOutloop dnd goal ctx scroll =
  let
    newCtx =
      let
        zOutloop =
          mkZOutloop
            { metadata = scroll.metadata
            , name = scroll.name
            , justif = scroll.justif
            , interaction = scroll.data.interaction }
            scroll.data.inloops
      in
      { zipper = zOutloop :: ctx.zipper
      , polarity = invert ctx.polarity }

    clickAction =
      case goal.actionMode of
        ProofMode Interacting ->
          case getSingleInloop goal.execMode ctx scroll of
            Nothing -> []
            Just (id, _) ->
              viewAction goal
                (Close { ctx = ctx, scroll = scroll, id = id })
                orangeActionable "Close"
        
        EditMode { interaction } ->
          case interaction of
            Operating -> deleteValAction goal ctx (valFromScroll scroll) 
            _ -> []

        _ ->
          (actionable Utils.Color.transparent).inactive
    
    paddingSize =
      case goal.location of
        App -> 10
        Manual _ -> 5
  in
  el
    ( [ width fill
      , height fill
      , Border.rounded scrollBorderRound ] )
    ( el
        ( [ width fill
          , height fill
          , padding paddingSize ]
         ++ clickAction )
        ( viewNet dnd goal newCtx scroll.data.outloop ) )


viewInloop : ValDnD -> Goal -> Context -> ScrollVal -> (Scroll.Ident, Env) -> Element Msg
viewInloop dnd goal ctx scroll (id, env) =
  let
    newCtx =
      let
        zScrollData =
          { metadata = scroll.metadata
          , name = scroll.name
          , justif = scroll.justif
          , interaction = scroll.data.interaction }
        zInloop =
          { scroll = zScrollData
          , outloop = scroll.data.outloop
          , neighbors = Dict.remove id scroll.data.inloops
          , metadata = env.metadata
          , justif = env.justif
          , id = id }
      in
      { ctx | zipper = ZInloop zInloop :: ctx.zipper }

    clickAction =
      case goal.actionMode of
        EditMode { interaction } ->
          case interaction of
            Operating ->
              deleteEnvAction goal ctx scroll id env
            _ ->
              []
        _ ->
          []

    paddingSize =
      case goal.location of
        App -> 10
        Manual _ -> 10
  in
  el
    [ width fill
    , height fill
    , Border.rounded scrollBorderRound
    , Background.color (scrollBackgroundColor ctx.polarity) ]
    ( el
        ( [ width fill
          , height fill
          , padding paddingSize ]
         ++ clickAction )
        ( viewNet dnd goal newCtx env.content ) )


addButton : ButtonParams msg -> Element msg
addButton params =
  let
    style =
      { width = Css.pct 100
      , height = Css.pct 100
      , color = Color.rgb255 58 134 255
      , iconColorEnabled = Color.white
      , iconColorDisabled = Color.darkGray }
  in
  button style params


viewAddInloopZone : Goal -> Context -> ScrollVal -> List (Element Msg)
viewAddInloopZone goal ctx scroll =
  let
    id =
      "Branch" ++ String.fromInt (Dict.size scroll.data.inloops + 1)

    insertEnvAction =
      InsertEnv { ctx = ctx, scroll = scroll, id = id, content = [] }

    addInloopButton =
        ( addButton
            { action =  Msg (Apply goal.location insertEnvAction)
            , title = "Insert new branch"
            , icon = Icons.plusSquare
            , enabled = True } )
  in
  case goal.actionMode of
    EditMode _ ->
      case applicable goal insertEnvAction of
        Ok _ ->
          [ el
              [ width shrink
              , height fill
              , padding 10
              , Border.rounded scrollBorderRound
              , Background.color (scrollBackgroundColor (invert ctx.polarity)) ]
              ( addInloopButton ) ]
        Err _ ->
          []
    _ ->
      []


viewAddValZone : Location -> Context -> String -> Element Msg
viewAddValZone location ctx newAtomName =
  let
    newVal =
      if String.isEmpty newAtomName then
        mkFakeScroll [][("Return", [])]
      else
        case newAtomName of
          "sugar" ->
            mkFakeFormula sugar

          "mascarpone" ->
            mkFakeFormula mascarpone

          "egg" ->
            mkFakeFormula egg

          "white" ->
            mkFakeFormula white

          "yolk" ->
            mkFakeFormula yolk

          "whisked whites" ->
            mkFakeFormula whiskedWhites

          "yolky paste" ->
            mkFakeFormula yolkPaste

          "thick paste" ->
            mkFakeFormula thickPaste

          "mascarpone cream" ->
            mkFakeFormula mascarponeCream

          _ ->
            mkFakeFormula (Formula.atom newAtomName)

    insertValAction =
      InsertVal { ctx = ctx
                , val = newVal }
    
    addValButton =
      el
        [ width fill
        , height fill ]
        ( addButton
            { action = Msg (Apply location insertValAction)
            , title = "Insert new value"
            , icon = Icons.plus
            , enabled = True } )
  in
  column
    [ width shrink
    , height fill
    , centerX
    , Border.rounded scrollBorderRound
    , Background.color Style.transparent ]
    [ addValButton ]


viewVal : ValDnD -> Goal -> Context -> Val -> Element Msg
viewVal dnd goal ctx val =
  case val.shape of
    Formula formula ->
      viewFormula dnd goal ctx val.metadata val.name val.justif formula
    
    Scroll ({ inloops } as scrollData) ->
      let
        scroll =
          { metadata = val.metadata
          , name = val.name
          , justif = val.justif
          , data = scrollData }

        outloopEl =
          viewOutloop dnd goal ctx scroll

        addInloopZone =
          viewAddInloopZone goal ctx scroll
        
        inloopsEl =
          row
            [ width fill
            , height fill
            , spacing scrollBorderWidth ]
            ( ( inloops |>
                Dict.toList |>
                List.map (viewInloop dnd goal ctx scroll) )
              ++ addInloopZone )

        color =
          case goal.actionMode of
            ProofMode _ -> useColor
            EditMode _ -> reorderColor
            _ -> Utils.Color.transparent
        
        { shadowOffset, shadowSize, shadowBlur, shadowAlpha } =
          case goal.location of
            App ->
              { shadowOffset = (0, 5)
              , shadowSize = 0.25
              , shadowBlur = 15
              , shadowAlpha = 1
              }
            Manual _ ->
              { shadowOffset = (0, 3)
              , shadowSize = 0.25
              , shadowBlur = 10
              , shadowAlpha = 0.7
              }
      in
      column
        ( [ width fill
          , height fill
          , Background.color (scrollForegroundColor ctx.polarity) ]
         ++ (List.map htmlAttribute <| DnD.droppable DragDropMsg Nothing)
         ++ View.Events.dragAction color dnd goal.location ctx val
         ++ onClick DoNothing
         :: Border.solid
         :: Border.width grownBorder.borderWidth
         :: drawGrownBorder val.metadata.grown
         ++
          [ Border.shadow
              { offset = shadowOffset
              , size = shadowSize
              , blur = shadowBlur
              , color =
                  scrollForegroundColor ctx.polarity |>
                  Utils.Color.fromElement |>
                  Utils.Color.withAlpha shadowAlpha |>
                  Utils.Color.toElement } ] )
        [ outloopEl, inloopsEl ]


viewNet : ValDnD -> Goal -> Context -> Net -> Element Msg
viewNet dnd goal ctx net =
  let
    newCtx (left, right) =
      { ctx | zipper = mkZNet left right :: ctx.zipper }

    valEl (left, right) =
      viewVal dnd goal (newCtx (left, right))

    dropAttrs (left, right) content =
      List.map htmlAttribute <|
      DnD.droppable DragDropMsg
        ( Just { target = { zipper = (newCtx (left, right)).zipper
                          , polarity = ctx.polarity }
               , content = content, location = goal.location } )
    
    dropAction (left, right) =
      case goal.actionMode of
        ProofMode Justifying ->
          case DnD.getDragId dnd of
            Nothing -> []
            Just { source, content } ->
              let
                iterateValAction =
                  IterateVal { srcCtx = source, srcVal = content
                             , tgtCtx = ctx, tgtName = Nothing }
              in
              case applicable goal iterateValAction of
                Err _ -> []
                Ok _ ->
                  let
                    dropStyle =
                      droppable useColor

                    dropTargetStyle =
                      case DnD.getDropId dnd of
                        Just (Just { target }) ->
                          if (newCtx (left, right)).zipper == target.zipper
                          then dropStyle.active
                          else dropStyle.inactive
                        _ ->
                          dropStyle.inactive
                  in
                  dropTargetStyle ++ dropAttrs (left, right) []

        EditMode { interaction } ->
          case interaction of
            Reordering ->
              case DnD.getDragId dnd of
                Just { source, content } ->
                  case source.zipper of
                    ZNet srcNet :: srcParent ->
                      if srcParent == ctx.zipper then
                        let
                          srcIdx = List.length srcNet.left
                          tgtIdx = List.length left
                        in
                        if tgtIdx == srcIdx || tgtIdx == srcIdx + 1 then []
                        else
                          let
                            whole = srcNet.left ++ content :: srcNet.right
                            newNet = Utils.List.move srcIdx tgtIdx whole

                            dropStyle =
                              droppable reorderColor

                            dropTargetStyle =
                              case DnD.getDropId dnd of
                                Just (Just { target }) ->
                                  if (newCtx (left, right)).zipper == target.zipper
                                  then dropStyle.active
                                  else dropStyle.inactive
                                _ ->
                                  dropStyle.inactive
                          in
                          dropTargetStyle ++ dropAttrs (left, right) newNet
                      else
                        []
                    _ ->
                      []
                _ ->
                  []
            _ ->
              []
        _ ->
          []

    spaceSize =
      case goal.location of
        App -> 30
        Manual _ -> 10
    
    layoutAttrs =
      [ (width (fill |> minimum spaceSize))
      , (height (fill |> minimum spaceSize))
      , padding spaceSize
      , spacing spaceSize ]
     
    borderAttrs =
      [ Border.width (droppable Utils.Color.transparent).borderWidth
      , Border.color Style.transparent ]

    length val =
      case val.shape of
        Formula _ -> shrink
        Scroll _ -> fill

    intersticial () =
      let
        attrs =
          layoutAttrs

        dropZone lr =
          el
            ( [ width (spaceSize |> px)
              , height (fill |> minimum spaceSize) ]
            ++ borderAttrs
            ++ dropAction lr )
            none

        sperse ((left, right) as lr) val =
          let
            lastDropzone =
              if List.isEmpty right
              then [onRight (dropZone (left ++ [val], right))]
              else []
          in
          el
            ( [ width (length val)
              , height fill
              , centerX, centerY
              , onLeft (dropZone (left, val :: right)) ]
              ++ lastDropzone )
            ( valEl lr val )

        els =
          Utils.List.zipperMap sperse net
        
        addValZone =
          case goal.actionMode of 
            EditMode { newAtomName } ->
              let
                insertValAction =
                  InsertVal { ctx = ctx, val = a"dummy" }
              in
              case applicable goal insertValAction of
                Ok _ ->
                  [viewAddValZone goal.location ctx newAtomName]
                Err _ ->
                  []
            _ ->
              []
      in
      wrappedRow attrs (els ++ addValZone)
        
    normal () =
      let
        attrs =
          layoutAttrs ++
          borderAttrs ++
          dropAction (net, [])
        
        sperse lr flower =
          el
            [ width (length flower)
            , height fill
            , centerX, centerY ]
            ( valEl lr flower )

        els =
          Utils.List.zipperMap sperse net
      in
      wrappedRow attrs els
  in
  case goal.actionMode of
    EditMode _ ->
      intersticial ()

    _ ->
      normal ()


inEditMode : Goal -> Bool
inEditMode { actionMode } =
  case actionMode of
    EditMode _ -> True
    _ -> False


viewGoal : ValDnD -> Goal -> Element Msg
viewGoal dnd goal =
  let
    congratsFontSize =
      case goal.location of
        App -> 48
        Manual _ -> 32
        
    goalHeight =
      height (fillPortion 4)

    goalEl () =
      if List.isEmpty goal.focus && not (inEditMode goal) then
        el [width fill, goalHeight]
          ( el
              [ centerX, centerY
              , Font.size congratsFontSize
              , Font.color
                  ( Utils.Color.fromRgb { red = 0.6, green = 0.6, blue = 0.6 } |>
                    Utils.Color.toElement )
              , padding 20
              ]
              ( text "Proof complete!" )
          )
      else
        el
          [ scrollbars
          , width fill
          , goalHeight ]
          ( viewNet dnd goal emptyCtx goal.focus )
  in
  case goal.actionMode of
    ProofMode _ ->
      goalEl ()
    
    EditMode _ ->
      goalEl ()

    _ ->
      el [goalHeight, centerX] (Widgets.fullPageTextMessage "Working on it!")