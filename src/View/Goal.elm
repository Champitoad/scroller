module View.Goal exposing (..)

import View.Style as Style exposing (..)
import View.Widgets exposing (..)
import View.Events exposing (..)

import Model.Formula as Formula exposing (..)
import Model.Scroll exposing (..)
import Model.Mascarpone exposing (..)
import Model.Goal exposing (..)
import Model.App exposing (..)

import Update.Rules exposing (..)
import Update.App exposing (..)

import Utils.List
import Utils.Events
import Utils.Color

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input

import Html.Attributes exposing (title)

import Html5.DragDrop as DnD

import Css

import FeatherIcons as Icons

import Color
import Utils.Color
import Utils.Events exposing (onClick)
import View.Widgets as Widgets


reorderColor : Color.Color
reorderColor =
  Utils.Color.fromRgb { red = 0.7, green = 0.7, blue = 0.7 }


importColor : Color.Color
importColor =
  Utils.Color.fromRgb { red = 1, green = 0.8, blue = 0 }


cropAction : Location -> Context -> Flower -> List (Attribute Msg)
cropAction location context flower =
  let actionableStyle = redActionable in
  if croppable context || isGrownFlower flower then
    Utils.Events.onClick (Action Crop location context.zipper [flower])
    :: (htmlAttribute <| title "Remove Flower")
    :: actionableStyle.active
  else
    actionableStyle.inactive


pullAction : Location -> Context -> Net -> List (Attribute Msg)
pullAction location context (Net netData) =
  let actionableStyle = redActionable in
  if pullable context then
    Utils.Events.onClick (Action Pull location context.zipper netData.flowers)
    :: (htmlAttribute <| title "Remove Inloop")
    :: actionableStyle.active
  else
    actionableStyle.inactive


drawGrownBorder : Bool -> List (Attribute msg)
drawGrownBorder doit =
  if doit then grownBorder.active else grownBorder.inactive


viewAtom : Ident -> Element Msg
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


viewFormula : FlowerDnD -> Goal -> FormulaData -> Element Msg
viewFormula dnd { mode, navigation, location } ({ metadata, statement } as data) =
  let
    form =
      Formula data
    
    context =
      current navigation

    clickAction =
      let
        actionableStyle =
          case statement of
            Atom _ -> greenActionable
            _ -> pinkActionable
      in
      case mode of
        ProofMode Argumenting ->
          case statement of
            Atom _ ->
              if isHypothesis form context.zipper then
                (Events.onClick (Action Justify location context.zipper [form]))
                :: (htmlAttribute <| title "Justify")
                :: actionableStyle.active
              else
                actionableStyle.inactive
            
            _ ->
              (Events.onClick (Action Decompose location context.zipper [form]))
              :: (htmlAttribute <| title "Decompose")
              :: actionableStyle.active
        
        EditMode Operating _ ->
          cropAction location context (Formula data)

        _ ->
          actionableStyle.inactive
    
    reorderDragAction =
      case mode of
        EditMode _ _ ->
          dragAction reorderColor dnd location context.zipper form
        
        _ ->
          []
    
    (fontSize, paddingSize) =
      case location of
        App -> (45, 10)
        Manual _ -> (30, 5)
  in
  el
    ( [ centerX, centerY
      , padding paddingSize
      , Font.color (flowerForegroundColor context.polarity)
      , Font.size fontSize
      , nonSelectable ]
      ++ reorderDragAction
      ++ clickAction
      ++ drawGrownBorder metadata.grown )
    ( viewStatement statement )


viewOutloop : FlowerDnD -> Goal -> OutloopData -> Net -> Element Msg
viewOutloop dnd ({ mode, navigation, location } as goal) { metadata, outloopMetadata, inloops } outloop =
  let
    context =
      current navigation

    newZipper =
      mkOutloop metadata outloopMetadata inloops :: context.zipper

    clickAction =
      case mode of
        ProofMode Argumenting ->
          let
            actionableStyle = orangeActionable

            action rule name =
              (Events.onClick (Action rule location newZipper outloop))
              :: (htmlAttribute <| title name)
              :: actionableStyle.active
          in
          if List.isEmpty outloop then
            case context.zipper of
              _ :: Outloop _ :: _ ->
                let
                  (rule, name) =
                    case List.length inloops of
                      0 -> (Close, "Ex falso quodlibet")
                      1 -> (Unlock, "Unlock")
                      _ -> (Case, "Case")
                in
                action rule name
              _ ->
                if List.length inloops == 1 then
                  action Unlock "Unlock"
                else
                  actionableStyle.inactive
          else
            actionableStyle.inactive
        
        EditMode Operating _ ->
          cropAction location context (mkFlower metadata outloop inloops)

        _ ->
          (actionable Utils.Color.transparent).inactive
    
    paddingSize =
      case location of
        App -> 10
        Manual _ -> 5
  in
  el
    ( [ width fill
      , height fill
      , Border.rounded flowerBorderRound ] )
    ( el
        ( [ width fill
          , height fill
          , padding paddingSize ]
         ++ clickAction )
        ( viewNet dnd
            { goal | navigation = changeFocus
              { context
              | zipper = newZipper
              , polarity = invert context.polarity
              } goal.navigation
            }
            outloop ) )


viewInloop : FlowerDnD -> Goal -> InloopData -> Net -> Element Msg
viewInloop dnd
  ({ mode, navigation, location } as goal)
  { metadata, inloopMetadata, outloop, left, right }
  (Net inloopData as inloop) =
  let
    context =
      current navigation
    
    newZipper =
      mkInloop metadata inloopMetadata outloop left right :: context.zipper

    clickAction =
      let actionableStyle = greenActionable in
      case mode of
        ProofMode Argumenting ->
          if List.isEmpty inloopData.flowers then
            (Events.onClick (Action Close location newZipper inloopData.flowers))
            :: (htmlAttribute <| title "QED")
            :: actionableStyle.active
          else
            actionableStyle.inactive
        
        EditMode Operating _ ->
          pullAction location { context | zipper = newZipper } inloop

        _ ->
          actionableStyle.inactive

    paddingSize =
      case location of
        App -> 10
        Manual _ -> 10
  in
  el
    [ width fill
    , height fill
    , Border.rounded flowerBorderRound
    , Background.color (flowerBackgroundColor context.polarity) ]
    ( el
        ( [ width fill
          , height fill
          , padding paddingSize ]
         ++ clickAction )
        ( viewNet dnd
            { goal | navigation = changeFocus { context | zipper = newZipper } goal.navigation }
            inloop ) )


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


viewAddInloopZone : Location -> Context -> FlowerData -> Element Msg
viewAddInloopZone location context { metadata, outloop, inloops } =
  let
    newFlower =
      mkFlower metadata outloop (inloops ++ [mkFakeNet []])

    addInloopButton =
        ( addButton
            { action =  Msg (Action Grow location context.zipper [newFlower])
            , title = "Add Inloop"
            , icon = Icons.plus
            , enabled = True } )
  in
  column
    [ width shrink
    , height fill
    , padding 10
    , Border.rounded flowerBorderRound
    , Background.color (flowerBackgroundColor (invert context.polarity)) ]
    [ addInloopButton ]


viewAddFlowerZone : Location -> Context -> String -> Net -> Element Msg
viewAddFlowerZone location context newAtomName flowers =
  let
    newFlower =
      if String.isEmpty newAtomName then
        mkFakeFlower (mkFakeNet []) [mkFakeNet []]
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

    newZipper newName =
      case context.zipper of
        [] -> [mkNet flowers []]
        zip :: zipper ->
          case zip of
            Net { left, right } ->
              mkNet (left ++ flowers) right :: zipper
            Outloop ({ outloopMetadata } as data) ->
              mkNet flowers [] ::
              Outloop { data | outloopMetadata = { outloopMetadata | newAtomName = newName } } ::
              zipper
            Inloop ({ inloopMetadata } as data) ->
              mkNet flowers [] ::
              InloopData { data | inloopMetadata = { inloopMetadata | newAtomName = newName } } ::
              zipper
    
    onChange newName =
      SetGoal (fillZipper [] (newZipper newName))
    
    addAtomTextEdit =
      Input.text
        [ width (105 |> px)
        , Border.rounded flowerBorderRound
        , onClick DoNothing ]
        { onChange = onChange
        , text = newAtomName
        , placeholder = Just (Input.placeholder [] (text "flower"))
        , label = Input.labelHidden "Atom name" }

    addFlowerButton =
      el
        [ width fill
        , height fill ]
        ( addButton
            { action = Msg (Action Grow location (newZipper newAtomName) [newFlower])
            , title = "Add Flower"
            , icon = Icons.plus
            , enabled = True } )
  in
  column
    [ width shrink
    , height fill
    , centerX
    , Border.rounded flowerBorderRound
    , Background.color Style.transparent ]
    [ addAtomTextEdit
    , addFlowerButton ]


viewFlower : FlowerDnD -> Goal -> Flower -> Element Msg
viewFlower dnd ({ mode, navigation, location } as goal) flower =
  case flower of
    Formula formula ->
      viewFormula dnd goal formula
    
    Flower ({ metadata, outloop, inloops } as data) ->
      let
        context =
          current navigation

        (Net outloopData) = outloop

        outloopEl =
          viewOutloop dnd goal (OutloopData metadata outloopData.metadata inloops) outloop
        
        addInloopZone =
          case mode of 
            EditMode _ _ ->
              if glueable context || data.metadata.grown then
                [viewAddInloopZone location context data]
              else
                []
            _ ->
              []
        
        inloopsEl =
          row
            [ width fill
            , height fill
            , spacing flowerBorderWidth ]
            ( Utils.List.zipperMap
                (\(left, right) (Net inloopData as inloop) ->
                  viewInloop dnd goal (InloopData metadata inloopData.metadata outloop left right) inloop)
                inloops
              ++ addInloopZone )

        color =
          case mode of
            ProofMode _ -> importColor
            EditMode _ _ -> reorderColor
            _ -> Utils.Color.transparent
        
        { shadowOffset, shadowSize, shadowBlur, shadowAlpha } =
          case location of
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
          , Background.color (flowerForegroundColor context.polarity) ]
         ++ (List.map htmlAttribute <| DnD.droppable DragDropMsg Nothing)
         ++ dragAction color dnd location context.zipper flower
         ++ onClick DoNothing
         :: Border.solid
         :: Border.width grownBorder.borderWidth
         :: drawGrownBorder metadata.grown
         ++
          [ Border.shadow
              { offset = shadowOffset
              , size = shadowSize
              , blur = shadowBlur
              , color =
                  flowerForegroundColor context.polarity |>
                  Utils.Color.fromElement |>
                  Utils.Color.withAlpha shadowAlpha |>
                  Utils.Color.toElement } ] )
        [ outloopEl, inloopsEl ]


viewNet : FlowerDnD -> Goal -> String -> Net -> Element Msg
viewNet dnd ({ mode, navigation, location } as goal) newAtomName net =
  let
    context =
      current navigation

    flowerEl (left, right) =
      viewFlower dnd
        { goal | navigation = changeFocus
          { context | zipper =
            mkNet left right :: context.zipper
          } goal.navigation
        }
    
    dropAction (left, right) =
      case mode of
        ProofMode Importing ->
          case DnD.getDragId dnd of
            Just { source } ->
              -- if isHypothesis content context.zipper then
              if justifies source context.zipper then
                let
                  dropStyle =
                    droppable importColor

                  dropTargetStyle =
                    case DnD.getDropId dnd of
                      Just (Just { target }) ->
                        if mkNet left right :: context.zipper == target
                        then dropStyle.active
                        else dropStyle.inactive
                    
                      _ ->
                        dropStyle.inactive
                in
                dropTargetStyle ++
                ( List.map htmlAttribute <|
                  DnD.droppable DragDropMsg
                    (Just { target = mkNet left right :: context.zipper
                          , content = [], location = location }) )
              else
                []
            _ ->
              []

        EditMode Reordering _ ->
          case DnD.getDragId dnd of
            Just { source, content } ->
              case source of
                Net sourceNet :: sourceParent ->
                  if sourceParent == context.zipper then
                    let
                      (sourceIndex, index) =
                        (List.length sourceNet.left, List.length left)
                    in
                    if sourceIndex == index || index == sourceIndex + 1 then []
                    else
                      let
                        whole =
                          left ++ right

                        newNet =
                          if sourceIndex < index then
                            let
                              middle =
                                Utils.List.slice (sourceIndex + 1) (index - 1) whole
                            in
                            sourceNet.left ++ middle ++ content :: right
                          else
                            let
                              middle =
                                Utils.List.slice index (sourceIndex - 1) whole
                            in
                            left ++ content :: (middle ++ sourceNet.right)

                        dropStyle =
                          droppable reorderColor

                        dropTargetStyle =
                          case DnD.getDropId dnd of
                            Just (Just { target }) ->
                              if mkNet left right :: context.zipper == target
                              then dropStyle.active
                              else dropStyle.inactive

                            _ ->
                              dropStyle.inactive
                      in
                      dropTargetStyle ++
                      ( List.map htmlAttribute <|
                        DnD.droppable DragDropMsg
                          (Just { target = mkNet left right :: context.zipper
                                , content = newNet, location = location }) )
                  else
                    []
                _ ->
                  []
            _ ->
              []
        _ ->
          []

    spaceSize =
      case location of
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

    length flower =
      case flower of
        Formula _ -> shrink
        Flower _ -> fill

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

        sperse ((left, right) as lr) flower =
          let
            lastDropzone =
              if List.isEmpty right
              then [onRight (dropZone (left ++ [flower], right))]
              else []
          in
          el
            ( [ width (length flower)
              , height fill
              , centerX, centerY
              , onLeft (dropZone (left, flower :: right)) ]
              ++ lastDropzone )
            ( flowerEl lr flower )

        els =
          Utils.List.zipperMap sperse net
        
        addFlowerZone =
          case mode of 
            EditMode _ _ ->
              if growable context then
                [viewAddFlowerZone location context newAtomName net]
              else
                []
            _ ->
              []
      in
      wrappedRow attrs (els ++ addFlowerZone)
        
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
            ( flowerEl lr flower )

        els =
          Utils.List.zipperMap sperse net
      in
      wrappedRow attrs els
  in
  case mode of
    EditMode _ _ ->
      intersticial ()

    _ ->
      normal ()


viewNet : FlowerDnD -> Goal -> Net -> Element Msg
viewNet dnd goal (Net { metadata, flowers }) =
  el
    ( fillXY ++
      drawGrownBorder metadata.grown )
    ( viewNet dnd goal metadata.newAtomName flowers )


inEditMode : Goal -> Bool
inEditMode { mode } =
  case mode of
    EditMode _ _ -> True
    _ -> False


viewGoal : FlowerDnD -> Goal -> Element Msg
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
          ( viewNet dnd goal "" goal.focus )
  in
  case goal.mode of
    ProofMode _ ->
      goalEl ()
    
    EditMode _ _ ->
      goalEl ()

    _ ->
      el [goalHeight, centerX] (Widgets.fullPageTextMessage "Working on it!")