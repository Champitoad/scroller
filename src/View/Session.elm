module View.Session exposing (..)

import Color
import Css
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons as Icons
import Html.Attributes exposing (title)
import Html5.DragDrop as DnD
import Model.App exposing (..)
import Model.Formula as Formula exposing (..)
import Model.Mascarpone exposing (..)
import Model.Scroll exposing (..)
import Model.Session as Session exposing (..)
import Update.App exposing (..)
import Utils.Color exposing (fromElement)
import Utils.Events exposing (onClick)
import Utils.List
import View.Events
import View.Style as Style exposing (..)
import View.Toolbar exposing (toolbarHeight)
import View.Widgets as Widgets exposing (..)


viewClickAction : Session -> Action -> ZoneStyle Msg -> String -> List (Attribute Msg)
viewClickAction session action actionableStyle titleText =
    case applicable action session of
        Err _ ->
            actionableStyle.inactive

        Ok _ ->
            Utils.Events.onClick (Apply session.route action)
                :: (htmlAttribute <| title titleText)
                :: actionableStyle.active


deleteAction : Session -> Id -> List (Attribute Msg)
deleteAction session id =
    viewClickAction session
        (Delete id)
        redActionable
        "Delete"


drawGrownBorder : Bool -> List (Attribute msg)
drawGrownBorder doit =
    if doit then
        grownBorder.active

    else
        grownBorder.inactive


viewNode : DnD -> Session -> Tree -> Element Msg
viewNode dnd session (TNode { id, node, children }) =
    let
        statusBar =
            let
                nameEl =
                    text node.name

                polarity =
                    getPolarity id session.net

                originName =
                    case node.justif.from of
                        Just origin ->
                            getName (originSourceId origin) session.net

                        _ ->
                            ""

                introIndicator =
                    if isInsertion polarity node.justif then
                        insertionIndicator

                    else if isIteration polarity node.justif then
                        iterationIndicator originName

                    else if isExpandedInloop id session.net then
                        expansionIndicator

                    else
                        none

                elimIndicator =
                    if isDeletion polarity node.justif then
                        deletionIndicator

                    else if isDeiteration polarity node.justif then
                        deiterationIndicator originName

                    else if isCollapsedInloop id session.net then
                        collapseIndicator

                    else
                        none
            in
            row
                [ width fill ]
                [ el [ alignLeft ] introIndicator
                , el [ centerX ] nameEl
                , el [ alignRight ] elimIndicator
                ]

        nodeShapeEl =
            case node.shape of
                Formula form ->
                    viewFormula dnd session id form

                Sep _ interaction ->
                    viewSep dnd session id interaction children
    in
    column
        [ width shrink, height shrink ]
        [ statusBar, nodeShapeEl ]


viewAtom : Formula.Ident -> Element Msg
viewAtom ident =
    case ident of
        Name name ->
            text name

        Image data ->
            image [ width (px 100), height (px 100) ] data


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
            row [] [ viewStatement f1, text " ∧ ", viewStatement f2 ]

        Or f1 f2 ->
            row [] [ viewStatement f1, text " v ", viewStatement f2 ]

        Implies f1 f2 ->
            row [] [ text "(", viewStatement f1, text " ⇒ ", viewStatement f2, text ")" ]

        Not f1 ->
            row [] [ text "¬ (", viewStatement f1, text ")" ]


viewFormula : DnD -> Session -> Id -> Formula -> Element Msg
viewFormula dnd session id formula =
    let
        clickAction =
            case session.actionMode of
                ProofMode Interacting ->
                    case formula of
                        Atom _ ->
                            []

                        _ ->
                            viewClickAction session
                                (Decompose id)
                                pinkActionable
                                "Decompose"

                EditMode { interaction } ->
                    case interaction of
                        Operating ->
                            deleteAction session id

                        _ ->
                            []

                _ ->
                    []

        dragAction =
            case session.actionMode of
                ProofMode Interacting ->
                    View.Events.dragAction useColor dnd session.route id

                EditMode _ ->
                    View.Events.dragAction reorderColor dnd session.route id

                _ ->
                    []

        ( fontSize, paddingSize ) =
            case session.route of
                Playground ->
                    ( 45, 10 )

                Manual _ ->
                    ( 30, 5 )
    in
    el
        ([ centerX
         , centerY
         , padding paddingSize
         , Font.color (scrollForegroundColor (getPolarity id session.net))
         , Font.size fontSize
         ]
            ++ dragAction
            ++ clickAction
        )
        (viewStatement formula)


viewOutloop : DnD -> Session -> Id -> List Tree -> Element Msg
viewOutloop dnd session id content =
    let
        clickAction =
            case session.actionMode of
                ProofMode Interacting ->
                    viewClickAction session (Close id) orangeActionable "Close"

                EditMode { interaction } ->
                    case interaction of
                        Operating ->
                            deleteAction session id

                        _ ->
                            []

                _ ->
                    (actionable Utils.Color.transparent).inactive

        paddingSize =
            case session.route of
                Playground ->
                    10

                Manual _ ->
                    5
    in
    el
        [ width fill
        , height fill
        , Border.rounded scrollBorderRound
        ]
        (el
            ([ width fill
             , height fill
             , padding paddingSize
             ]
                ++ clickAction
            )
            (viewTrees dnd session (Inside id) content)
        )


viewInloop : DnD -> Session -> Id -> List Tree -> Element Msg
viewInloop dnd session id content =
    let
        clickAction =
            case session.actionMode of
                EditMode { interaction } ->
                    case interaction of
                        Operating ->
                            deleteAction session id

                        _ ->
                            []

                _ ->
                    []

        paddingSize =
            case session.route of
                Playground ->
                    10

                Manual _ ->
                    10
    in
    el
        [ width fill
        , height fill
        , Border.rounded scrollBorderRound
        , Background.color (scrollBackgroundColor (getPolarity id session.net))
        ]
        (el
            ([ width fill
             , height fill
             , padding paddingSize
             ]
                ++ clickAction
            )
            (viewTrees dnd session (Inside id) content)
        )


addButton : IconButtonParams msg -> Element msg
addButton params =
    let
        style =
            { width = Css.pct 100
            , height = Css.pct 100
            , color = introColor |> fromElement
            , iconColorEnabled = Color.white
            , iconColorDisabled = Color.darkGray
            }
    in
    iconButton style params


viewAddInloopZone : Session -> Id -> List (Element Msg)
viewAddInloopZone session id =
    let
        pos =
            Dict.size (getOutloopInteractions id session.net) + 1

        loc =
            { ctx = Inside id, pos = pos }

        tok =
            ISep []

        insertAction =
            Insert loc tok

        insertEvent =
            case findAncestor (\id_ _ -> Session.isInserted id_ session) id session.net of
                Just ancId ->
                    Transform session.route (subInsert ancId loc tok)

                Nothing ->
                    Apply session.route insertAction

        addInloopButton =
            addButton
                { action = Msg insertEvent
                , title = "Insert new branch"
                , icon = Icons.plusSquare
                , enabled = True
                }
    in
    case session.actionMode of
        EditMode _ ->
            case applicable insertAction session of
                Ok _ ->
                    [ el
                        [ width shrink
                        , height fill
                        , padding 10
                        , Border.rounded scrollBorderRound
                        , Background.color (scrollBackgroundColor (invert (getPolarity id session.net)))
                        ]
                        addInloopButton
                    ]

                Err _ ->
                    []

        _ ->
            []


viewAddValZone : Location -> Context -> String -> Element Msg
viewAddValZone location ctx newAtomName =
    let
        newVal =
            if String.isEmpty newAtomName then
                mkFakeScroll [] [ ( "Return", [] ) ]

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
            InsertVal
                { ctx = ctx
                , val = newVal
                }

        addValButton =
            el
                [ width fill
                , height fill
                ]
                (addButton
                    { action = Msg (Apply location insertValAction)
                    , title = "Insert new value"
                    , icon = Icons.plus
                    , enabled = True
                    }
                )
    in
    column
        [ width shrink
        , height fill
        , centerX
        , Border.rounded scrollBorderRound
        , Background.color Style.transparent
        ]
        [ addValButton ]


viewSep : DnD -> Session -> Id -> Maybe Interaction -> List Tree -> Element Msg
viewSep dnd session id interaction children =
    let
        scroll =
            { metadata = val.metadata
            , name = val.name
            , justif = val.justif
            , data = scrollData
            }

        outloopEl =
            viewOutloop dnd session ctx scroll

        addInloopZone =
            viewAddInloopZone session ctx scroll

        inloopsEl =
            row
                [ width fill
                , height fill
                , spacing scrollBorderWidth
                ]
                ((inloops
                    |> Dict.toList
                    |> List.map (viewInloop dnd session ctx scroll)
                 )
                    ++ addInloopZone
                )

        color =
            case session.actionMode of
                ProofMode _ ->
                    useColor

                EditMode _ ->
                    reorderColor

                _ ->
                    Utils.Color.transparent

        { shadowOffset, shadowSize, shadowBlur, shadowAlpha } =
            case session.route of
                Playground ->
                    { shadowOffset = ( 0, 5 )
                    , shadowSize = 0.25
                    , shadowBlur = 15
                    , shadowAlpha = 1
                    }

                Manual _ ->
                    { shadowOffset = ( 0, 3 )
                    , shadowSize = 0.25
                    , shadowBlur = 10
                    , shadowAlpha = 0.7
                    }
    in
    column
        ([ width fill
         , height fill
         , Background.color (scrollForegroundColor ctx.polarity)
         ]
            ++ (List.map htmlAttribute <| DnD.droppable DragDropMsg Nothing)
            ++ View.Events.dragAction color dnd session.route ctx val
            ++ onClick DoNothing
            :: Border.solid
            :: Border.width grownBorder.borderWidth
            :: drawGrownBorder val.metadata.grown
            ++ [ Border.shadow
                    { offset = shadowOffset
                    , size = shadowSize
                    , blur = shadowBlur
                    , color =
                        scrollForegroundColor ctx.polarity
                            |> Utils.Color.fromElement
                            |> Utils.Color.withAlpha shadowAlpha
                            |> Utils.Color.toElement
                    }
               ]
        )
        [ outloopEl, inloopsEl ]


viewTrees : DnD -> Session -> Context -> List Tree -> Element Msg
viewTrees dnd session ctx trees =
    let
        newCtx ( left, right ) =
            { ctx | zipper = mkZNet left right :: ctx.zipper }

        valEl ( left, right ) =
            viewVal dnd session (newCtx ( left, right ))

        dropAttrs ( left, right ) content =
            List.map htmlAttribute <|
                DnD.droppable DragDropMsg
                    (Just
                        { target =
                            { zipper = (newCtx ( left, right )).zipper
                            , polarity = ctx.polarity
                            }
                        , content = content
                        , location = session.route
                        }
                    )

        dropAction ( left, right ) =
            case session.actionMode of
                ProofMode Justifying ->
                    case DnD.getDragId dnd of
                        Nothing ->
                            []

                        Just { source, content } ->
                            let
                                iterateValAction =
                                    IterateVal
                                        { srcCtx = source
                                        , srcVal = content
                                        , tgtCtx = ctx
                                        , tgtName = Nothing
                                        }
                            in
                            case applicable session iterateValAction of
                                Err _ ->
                                    []

                                Ok _ ->
                                    let
                                        dropStyle =
                                            droppable useColor

                                        dropTargetStyle =
                                            case DnD.getDropId dnd of
                                                Just (Just { target }) ->
                                                    if (newCtx ( left, right )).zipper == target.zipper then
                                                        dropStyle.active

                                                    else
                                                        dropStyle.inactive

                                                _ ->
                                                    dropStyle.inactive
                                    in
                                    dropTargetStyle ++ dropAttrs ( left, right ) []

                EditMode { interaction } ->
                    case interaction of
                        Reordering ->
                            case DnD.getDragId dnd of
                                Just { source, content } ->
                                    case source.zipper of
                                        (ZNet srcNet) :: srcParent ->
                                            if srcParent == ctx.zipper then
                                                let
                                                    srcIdx =
                                                        List.length srcNet.left

                                                    tgtIdx =
                                                        List.length left
                                                in
                                                if tgtIdx == srcIdx || tgtIdx == srcIdx + 1 then
                                                    []

                                                else
                                                    let
                                                        whole =
                                                            srcNet.left ++ content :: srcNet.right

                                                        newNet =
                                                            Utils.List.move srcIdx tgtIdx whole

                                                        dropStyle =
                                                            droppable reorderColor

                                                        dropTargetStyle =
                                                            case DnD.getDropId dnd of
                                                                Just (Just { target }) ->
                                                                    if (newCtx ( left, right )).zipper == target.zipper then
                                                                        dropStyle.active

                                                                    else
                                                                        dropStyle.inactive

                                                                _ ->
                                                                    dropStyle.inactive
                                                    in
                                                    dropTargetStyle ++ dropAttrs ( left, right ) newNet

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
            case session.route of
                Playground ->
                    30

                Manual _ ->
                    10

        layoutAttrs =
            [ width (fill |> minimum spaceSize)
            , height (fill |> minimum spaceSize)
            , padding spaceSize
            , spacing spaceSize
            ]

        borderAttrs =
            [ Border.width (droppable Utils.Color.transparent).borderWidth
            , Border.color Style.transparent
            ]

        length val =
            case val.shape of
                Formula _ ->
                    shrink

                Scroll _ ->
                    fill

        intersticial () =
            let
                attrs =
                    layoutAttrs

                dropZone lr =
                    el
                        ([ width (spaceSize |> px)
                         , height (fill |> minimum spaceSize)
                         ]
                            ++ borderAttrs
                            ++ dropAction lr
                        )
                        none

                sperse (( left, right ) as lr) val =
                    let
                        lastDropzone =
                            if List.isEmpty right then
                                [ onRight (dropZone ( left ++ [ val ], right )) ]

                            else
                                []
                    in
                    el
                        ([ width (length val)
                         , height fill
                         , centerX
                         , centerY
                         , onLeft (dropZone ( left, val :: right ))
                         ]
                            ++ lastDropzone
                        )
                        (valEl lr val)

                els =
                    Utils.List.zipperMap sperse net

                addValZone =
                    case session.actionMode of
                        EditMode { newAtomName } ->
                            let
                                insertValAction =
                                    InsertVal { ctx = ctx, val = a "dummy" }
                            in
                            case applicable session insertValAction of
                                Ok _ ->
                                    [ viewAddValZone session.route ctx newAtomName ]

                                Err _ ->
                                    []

                        _ ->
                            []
            in
            wrappedRow attrs (els ++ addValZone)

        normal () =
            let
                attrs =
                    layoutAttrs
                        ++ borderAttrs
                        ++ dropAction ( net, [] )

                sperse lr flower =
                    el
                        [ width (length flower)
                        , height fill
                        , centerX
                        , centerY
                        ]
                        (valEl lr flower)

                els =
                    Utils.List.zipperMap sperse net
            in
            wrappedRow attrs els
    in
    case session.actionMode of
        EditMode _ ->
            intersticial ()

        _ ->
            normal ()


inEditMode : Session -> Bool
inEditMode { actionMode } =
    case actionMode of
        EditMode _ ->
            True

        _ ->
            False


sessionHeightAttr : Attribute msg
sessionHeightAttr =
    styleAttr "height" ("calc(100vh - " ++ String.fromInt toolbarHeight ++ "px)")


viewSession : DnD -> Session -> Element Msg
viewSession dnd session =
    let
        congratsFontSize =
            case session.route of
                Playground ->
                    48

                Manual _ ->
                    32

        sessionEl () =
            if List.isEmpty session.net && not (inEditMode session) then
                el [ width fill, sessionHeightAttr ]
                    (el
                        [ centerX
                        , centerY
                        , Font.size congratsFontSize
                        , Font.color
                            (Utils.Color.fromRgb { red = 0.6, green = 0.6, blue = 0.6 }
                                |> Utils.Color.toElement
                            )
                        , padding 20
                        ]
                        (text "Proof complete!")
                    )

            else
                el
                    [ width fill
                    , sessionHeightAttr
                    , styleAttr "overflow-x" "hidden"
                    , styleAttr "overflow-y" "auto"
                    ]
                    (viewNet dnd session emptyCtx session.net)
    in
    case session.actionMode of
        ProofMode _ ->
            sessionEl ()

        EditMode _ ->
            sessionEl ()

        _ ->
            el [ sessionHeightAttr, centerX ] (Widgets.fullPageTextMessage "Working on it!")
