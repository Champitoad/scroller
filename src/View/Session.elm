module View.Session exposing (..)

import Color
import Css
import Css.Global exposing (children)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import FeatherIcons as Icons
import Html.Attributes exposing (title)
import Html5.DragDrop as DnD
import Model.App exposing (..)
import Model.Formula as Formula exposing (..)
import Model.Mascarpone exposing (..)
import Model.Scroll exposing (..)
import Model.Session as Session exposing (..)
import Update.App exposing (..)
import Utils.Color
import Utils.Events exposing (onClick)
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
                    el
                        [ foregroundColor node.polarity |> Utils.Color.elementAttr ]
                        (text node.name)

                polarity =
                    case session.execMode of
                        Forward ->
                            node.polarity

                        Backward ->
                            invert node.polarity

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

                    else if
                        getInloopInteraction id session.net
                            |> Maybe.map (isExpansion polarity)
                            |> Maybe.withDefault False
                    then
                        expansionIndicator

                    else
                        none

                elimIndicator =
                    if isDeletion polarity node.justif then
                        deletionIndicator

                    else if isDeiteration polarity node.justif then
                        deiterationIndicator originName

                    else if
                        getInloopInteraction id session.net
                            |> Maybe.map (isCollapse polarity)
                            |> Maybe.withDefault False
                    then
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

                Sep _ _ ->
                    viewSep dnd session id children
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
                    ( 35, 10 )

                Manual _ ->
                    ( 30, 5 )
    in
    el
        ([ centerX
         , centerY
         , padding paddingSize
         , foregroundColor (getPolarity id session.net) |> Utils.Color.elementAttr
         , Style.fontSize fontSize
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
        , styleAttr "border-radius" (String.fromInt scrollBorderRound ++ "px")
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
        , styleAttr "border-radius" (String.fromInt scrollBorderRound ++ "px")
        , Background.color (backgroundColor (getPolarity id session.net))
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
            , color = introColor |> Utils.Color.fromElement
            , iconColorEnabled = Color.white
            , iconColorDisabled = Color.darkGray
            }
    in
    iconButton style params


insertMsg : Session -> Location -> IToken -> Dict Id Int -> Msg
insertMsg session loc tok insertions =
    if existsAncestorContext (\ancId _ -> Dict.member ancId insertions) loc.ctx session.net then
        Transform session.route (Session.map (insert False loc tok))

    else
        Apply session.route (Insert loc tok)


viewAddInloopZone : Session -> Id -> List (Element Msg)
viewAddInloopZone session outloopId =
    let
        pos =
            Dict.size (getOutloopInteractions outloopId session.net)

        loc =
            { ctx = Inside outloopId, pos = pos }

        tok =
            ISep []

        addInloopButton insertions =
            addButton
                { action = Msg (insertMsg session loc tok insertions)
                , title = "Insert new branch"
                , icon = Icons.plusSquare
                , enabled = True
                }
    in
    case session.actionMode of
        EditMode { insertions } ->
            case applicable (Insert loc tok) session of
                Ok _ ->
                    [ el
                        [ width shrink
                        , height fill
                        , padding 10
                        , styleAttr "border-radius" (String.fromInt scrollBorderRound ++ "px")
                        , Background.color (backgroundColor (invert (getPolarity outloopId session.net)))
                        ]
                        (addInloopButton insertions)
                    ]

                Err _ ->
                    []

        _ ->
            []


viewAddOTokenZone : Session -> Context -> String -> Element Msg
viewAddOTokenZone session ctx newAtomName =
    let
        neighbors =
            getChildIdsContext ctx session.net

        loc =
            { ctx = ctx, pos = List.length neighbors }

        otoken =
            if String.isEmpty newAtomName then
                emptyScroll

            else
                case newAtomName of
                    "sugar" ->
                        OForm sugar

                    "mascarpone" ->
                        OForm mascarpone

                    "egg" ->
                        OForm egg

                    "white" ->
                        OForm white

                    "yolk" ->
                        OForm yolk

                    "whisked whites" ->
                        OForm whiskedWhites

                    "yolky paste" ->
                        OForm yolkPaste

                    "thick paste" ->
                        OForm thickPaste

                    "mascarpone cream" ->
                        OForm mascarponeCream

                    _ ->
                        OForm (Formula.atom newAtomName)

        tok =
            ITok otoken

        addOTokenButton insertions =
            el
                [ width fill
                , height fill
                ]
                (addButton
                    { action = Msg (insertMsg session loc tok insertions)
                    , title = "Insert new value"
                    , icon = Icons.plus
                    , enabled = True
                    }
                )
    in
    case ( session.actionMode, applicable (Insert loc tok) session ) of
        ( EditMode { insertions }, Ok _ ) ->
            column
                [ width shrink
                , height fill
                , centerX
                , styleAttr "border-radius" (String.fromInt scrollBorderRound ++ "px")
                , Background.color Style.transparent
                ]
                [ addOTokenButton insertions ]

        _ ->
            none


viewSep : DnD -> Session -> Id -> List Tree -> Element Msg
viewSep dnd session id children =
    let
        outloopContent =
            List.filter
                (\(TNode { node }) ->
                    case node.shape of
                        Sep _ (Just _) ->
                            False

                        _ ->
                            True
                )
                children

        outloopEl =
            viewOutloop dnd session id outloopContent

        addInloopZone =
            viewAddInloopZone session id

        inloopsEl =
            row
                [ width fill
                , height fill
                , spacing scrollBorderWidth
                ]
                ((children
                    |> List.filter (\(TNode child) -> isInloop child.id session.net)
                    |> List.map (\(TNode inloop) -> viewInloop dnd session inloop.id inloop.children)
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
         , Background.color (foregroundColor (getPolarity id session.net))
         ]
            ++ (List.map htmlAttribute <| DnD.droppable DragDropMsg Nothing)
            ++ View.Events.dragAction color dnd session.route id
            ++ onClick DoNothing
            :: styleAttr "border-style" "solid"
            :: styleAttr "border-width" (String.fromInt grownBorder.borderWidth ++ "px")
            :: drawGrownBorder (isInserted id session)
            ++ [ let
                    colorStr =
                        foregroundColor (getPolarity id session.net)
                            |> Utils.Color.fromElement
                            |> Utils.Color.withAlpha shadowAlpha
                            |> Color.toCssString

                    shadowStr =
                        String.fromFloat (Tuple.first shadowOffset)
                            ++ "px "
                            ++ String.fromFloat (Tuple.second shadowOffset)
                            ++ "px "
                            ++ String.fromFloat shadowBlur
                            ++ "px "
                            ++ String.fromFloat shadowSize
                            ++ "px "
                            ++ colorStr
                 in
                 styleAttr "box-shadow" shadowStr
               ]
        )
        [ outloopEl, inloopsEl ]


viewTrees : DnD -> Session -> Context -> List Tree -> Element Msg
viewTrees dnd session ctx trees =
    let
        treeEl tree =
            viewNode dnd session tree

        neighbors =
            getChildIdsContext ctx session.net

        dropAttrs dest =
            List.map htmlAttribute <|
                DnD.droppable DragDropMsg
                    (Just
                        { route = session.route
                        , destination = dest
                        }
                    )

        dropAction pos =
            case session.actionMode of
                ProofMode Justifying ->
                    case DnD.getDragId dnd of
                        Nothing ->
                            []

                        Just { source } ->
                            let
                                (DragNode src) =
                                    source

                                iterateAction =
                                    Iterate src { ctx = ctx, pos = List.length neighbors }
                            in
                            case applicable iterateAction session of
                                Err _ ->
                                    []

                                Ok _ ->
                                    let
                                        dropStyle =
                                            droppable useColor

                                        dropTargetStyle =
                                            case DnD.getDropId dnd of
                                                -- Hovering some droppable destination
                                                Just (Just { destination }) ->
                                                    case destination of
                                                        DropContext dropCtx ->
                                                            -- We only highlight when it's an (iterable) area
                                                            if dropCtx == ctx then
                                                                dropStyle.active

                                                            else
                                                                dropStyle.inactive

                                                        _ ->
                                                            dropStyle.inactive

                                                _ ->
                                                    dropStyle.inactive
                                    in
                                    dropTargetStyle ++ dropAttrs (DropContext ctx)

                EditMode { interaction } ->
                    case interaction of
                        Reordering ->
                            case DnD.getDragId dnd of
                                Just { source } ->
                                    let
                                        (DragNode src) =
                                            source

                                        srcCtx =
                                            getContext src session.net
                                    in
                                    if srcCtx == ctx then
                                        let
                                            srcPos =
                                                getPosition src session.net

                                            tgtPos =
                                                pos
                                        in
                                        if tgtPos == srcPos || tgtPos == srcPos + 1 then
                                            []

                                        else
                                            let
                                                loc =
                                                    { ctx = ctx, pos = pos }

                                                dropStyle =
                                                    droppable reorderColor

                                                dropTargetStyle =
                                                    case DnD.getDropId dnd of
                                                        Just (Just { destination }) ->
                                                            case destination of
                                                                DropLocation dropLoc ->
                                                                    if dropLoc == loc then
                                                                        dropStyle.active

                                                                    else
                                                                        dropStyle.inactive

                                                                _ ->
                                                                    dropStyle.inactive

                                                        _ ->
                                                            dropStyle.inactive
                                            in
                                            dropTargetStyle ++ dropAttrs (DropLocation loc)

                                    else
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
            [ styleAttr "border-width" (String.fromInt (droppable Utils.Color.transparent).borderWidth ++ "px")
            , styleAttr "border-color" "transparent"
            , styleAttr "border-style" "solid"
            ]

        elemLength node =
            case node.shape of
                Formula _ ->
                    shrink

                Sep _ _ ->
                    fill

        intersticial () =
            let
                attrs =
                    layoutAttrs

                dropZone pos =
                    el
                        ([ width (spaceSize |> px)
                         , height (fill |> minimum spaceSize)
                         ]
                            ++ borderAttrs
                            ++ dropAction pos
                        )
                        none

                sperse pos ((TNode { node }) as tree) =
                    let
                        lastDropzone =
                            if pos >= List.length neighbors then
                                [ onRight (dropZone pos) ]

                            else
                                []
                    in
                    el
                        ([ width (elemLength node)
                         , height fill
                         , centerX
                         , centerY
                         , onLeft (dropZone pos)
                         ]
                            ++ lastDropzone
                        )
                        (treeEl tree)

                els =
                    List.indexedMap sperse trees

                addOTokenZone =
                    case session.actionMode of
                        EditMode { newAtomName } ->
                            let
                                insertAction =
                                    Insert
                                        { ctx = ctx, pos = List.length neighbors }
                                        (ITok (OForm (Formula.atom newAtomName)))
                            in
                            case applicable insertAction session of
                                Ok _ ->
                                    [ viewAddOTokenZone session ctx newAtomName ]

                                Err _ ->
                                    []

                        _ ->
                            []
            in
            wrappedRow attrs (els ++ addOTokenZone)

        normal () =
            let
                attrs =
                    layoutAttrs
                        ++ borderAttrs
                        ++ dropAction (List.length neighbors)

                sperse ((TNode { node }) as tree) =
                    el
                        [ width (elemLength node)
                        , height fill
                        , centerX
                        , centerY
                        ]
                        (treeEl tree)

                els =
                    List.map sperse trees
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
        sessionEl () =
            let
                _ =
                    Debug.log "net" (stringOfNet session.net)
            in
            el
                [ width fill
                , sessionHeightAttr
                , styleAttr "overflow-x" "hidden"
                , styleAttr "overflow-y" "auto"
                ]
                (viewTrees dnd session TopLevel (hydrate session.net))
    in
    case session.actionMode of
        ProofMode _ ->
            sessionEl ()

        EditMode _ ->
            sessionEl ()

        _ ->
            el [ sessionHeightAttr, centerX ] (Widgets.fullPageTextMessage "Working on it!")
