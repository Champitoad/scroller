module View.Session exposing (..)

import Color
import Css
import Css.Global exposing (children)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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


drawInsertedBorder : Bool -> List (Attribute msg)
drawInsertedBorder doit =
    if doit then
        insertedBorder.active

    else
        insertedBorder.inactive


viewNode : DnD -> Session -> Tree -> Element Msg
viewNode dnd session ((TNode { id, node }) as tree) =
    let
        debug =
            False

        statusBar =
            let
                nameEl =
                    let
                        isRenaming =
                            case session.renaming of
                                Just renaming ->
                                    renaming.id == id

                                Nothing ->
                                    False

                        commonAttrs =
                            [ width (fill |> minimum 10)
                            , height (indicatorHeight - 2 |> px)
                            , foregroundColor node.polarity |> Utils.Color.elementAttr
                            , nameFontFamily
                            ]
                    in
                    if isRenaming then
                        Input.text
                            (commonAttrs
                                ++ [ padding 0
                                   , Background.color Style.transparent
                                   , Border.width 0
                                   , htmlAttribute (Html.Attributes.id "renaming-input")
                                   ]
                            )
                            { onChange = Rename id
                            , text = node.name
                            , placeholder = Nothing
                            , label = Input.labelHidden "Node name"
                            }

                    else
                        el
                            (commonAttrs
                                ++ [ Font.center
                                   , Utils.Events.onClick (StartRenaming id)
                                   , htmlAttribute (Html.Attributes.style "cursor" "text")
                                   ]
                            )
                            (text <|
                                (if debug then
                                    String.fromInt id ++ "#"

                                 else
                                    ""
                                )
                                    ++ node.name
                            )

                polarity =
                    case session.execMode of
                        Forward ->
                            node.polarity

                        Backward ->
                            invert node.polarity

                ( originId, originName, isHoveredOrigin ) =
                    case node.justif.from of
                        Just origin ->
                            let
                                oId =
                                    originSourceId origin
                            in
                            ( Just oId, getName oId session.net, session.hoveredOrigin == Just oId )

                        _ ->
                            ( Nothing, "", False )

                introIndicator =
                    if isInsertion polarity node.justif then
                        insertionIndicator

                    else if isIteration polarity node.justif then
                        iterationIndicator originId originName isHoveredOrigin

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
                        deiterationIndicator originId originName isHoveredOrigin

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
                [ width fill, spacing 10 ]
                [ el [ alignLeft ] introIndicator
                , el [ width fill ] nameEl
                , el [ alignRight ] elimIndicator
                ]

        ( nodeShapeEl, nodeHeight, nodeBorderRadius ) =
            case node.shape of
                Formula form ->
                    ( viewFormula dnd session id form
                    , height shrink
                    , styleAttr "border-radius" "none"
                    )

                Sep _ _ ->
                    ( viewSep dnd session tree
                    , height fill
                    , sepBorderRadius
                    )

        dragColor =
            case session.actionMode of
                ProofMode _ ->
                    useColor

                EditMode _ ->
                    reorderColor

                _ ->
                    Utils.Color.transparent

        drawHoveredOrigin =
            if session.hoveredOrigin == Just id then
                [ styleAttr "border-width" "3px"
                , styleAttr "border-color" (Color.toCssString Style.useColor)
                , styleAttr "border-style" "solid"
                ]

            else
                []

        drawErased =
            if Session.isErased id session then
                [ styleAttr "opacity" "0.5"
                , styleAttr "pointer-events" "none"
                ]

            else
                []

        dropAttrs dest =
            List.map htmlAttribute <|
                DnD.droppable DragDropMsg
                    (Just
                        { route = session.route
                        , destination = dest
                        }
                    )

        dropAction =
            case session.actionMode of
                ProofMode { copyMode } ->
                    if copyMode == Deiteration then
                        case DnD.getDragId dnd of
                            Nothing ->
                                []

                            Just { source } ->
                                let
                                    (DragNode src) =
                                        source

                                    deiterateAction =
                                        Deiterate src id
                                in
                                case applicable deiterateAction session of
                                    Err _ ->
                                        []

                                    Ok _ ->
                                        let
                                            dropStyle =
                                                droppableNode useColor

                                            dropTargetStyle =
                                                case DnD.getDropId dnd of
                                                    -- Hovering some droppable destination
                                                    Just (Just { destination }) ->
                                                        case destination of
                                                            DropNode dropId ->
                                                                -- We only highlight when it's a deiterable node
                                                                if dropId == id then
                                                                    dropStyle.active

                                                                else
                                                                    dropStyle.inactive

                                                            _ ->
                                                                dropStyle.inactive

                                                    _ ->
                                                        dropStyle.inactive
                                        in
                                        dropTargetStyle ++ dropAttrs (DropNode id)

                    else
                        []

                _ ->
                    []
    in
    column
        [ width fill, nodeHeight, centerX, centerY ]
        [ statusBar
        , el
            (View.Events.dragAction dragColor dnd session.route id
                ++ drawInsertedBorder (isInserted id session)
                ++ drawHoveredOrigin
                ++ drawErased
                ++ dropAction
                ++ [ width fill
                   , centerY
                   , nodeHeight
                   , nodeBorderRadius
                   , onClick DoNothing
                   ]
            )
            nodeShapeEl
        ]


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
                ProofMode { interaction } ->
                    if interaction == Interacting then
                        case formula of
                            Atom _ ->
                                []

                            _ ->
                                viewClickAction session
                                    (Decompose id)
                                    pinkActionable
                                    "Decompose"

                    else
                        []

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
                ProofMode { interaction } ->
                    if interaction == Interacting then
                        View.Events.dragAction useColor dnd session.route id

                    else
                        []

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
                ProofMode { interaction } ->
                    if interaction == Interacting then
                        viewClickAction session (Close id) orangeActionable "Close"

                    else
                        []

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
        ]
        (el
            ([ width fill
             , height fill
             , padding paddingSize
             ]
                ++ clickAction
            )
            (viewNodes dnd session (Inside id) content)
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


insertMsg : Session -> Location -> IToken -> Msg
insertMsg session loc tok =
    if existsAncestorContext (\ancId -> isInserted ancId session) loc.ctx session.net then
        Transform session.route (Session.map (insert False loc tok))

    else
        Apply session.route (Insert loc tok)


viewAddInloopZone : Session -> Id -> List (Element Msg)
viewAddInloopZone session outloopId =
    let
        pos =
            List.length (getChildIds outloopId session.net)

        loc =
            { ctx = Inside outloopId, pos = pos }

        tok =
            ISep []

        kindStr =
            case getPolarityContext loc.ctx session.net of
                Pos ->
                    "continuation"

                Neg ->
                    "branch"

        addInloopButton =
            addButton
                { action = Msg (insertMsg session loc tok)
                , title = "Insert new " ++ kindStr
                , icon = Icons.plusSquare
                , enabled = True
                }
    in
    case ( session.actionMode, applicable (Insert loc tok) session ) of
        ( EditMode _, Ok _ ) ->
            [ el
                [ width shrink
                , height fill
                , padding 10
                , sepBorderRadius
                , Background.color (backgroundColor (invert (getPolarity outloopId session.net)))
                ]
                addInloopButton
            ]

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

        kindStr =
            case getPolarityContext ctx session.net of
                Pos ->
                    "value"

                Neg ->
                    "parameter"

        addOTokenButton =
            el
                [ width fill
                , height fill
                ]
                (addButton
                    { action = Msg (insertMsg session loc tok)
                    , title = "Insert new " ++ kindStr
                    , icon = Icons.plus
                    , enabled = True
                    }
                )
    in
    case ( session.actionMode, applicable (Insert loc tok) session ) of
        ( EditMode _, Ok _ ) ->
            column
                [ width shrink
                , height fill
                , centerX
                , sepBorderRadius
                , Background.color Style.transparent
                ]
                [ addOTokenButton ]

        _ ->
            none


isDeiterationTarget : DnD -> Session -> Id -> Bool
isDeiterationTarget dnd session id =
    case DnD.getDragId dnd of
        Nothing ->
            False

        Just { source } ->
            let
                (DragNode src) =
                    source
            in
            applicable (Deiterate src id) session == Ok ()


viewSep : DnD -> Session -> Tree -> Element Msg
viewSep dnd session (TNode { id, node, children }) =
    let
        outloopContent =
            List.filter
                (\(TNode child) ->
                    case child.node.shape of
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

        inloops =
            List.filter (\(TNode child) -> isInloop child.id session.net) children

        inloopsEl =
            if List.length inloops == 0 then
                none

            else
                row
                    [ width fill
                    , height fill
                    , spacing sepBorderWidth
                    ]
                    (List.map (viewNode dnd session) inloops ++ addInloopZone)

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

        shadowColorStr =
            foregroundColor node.polarity
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
                ++ shadowColorStr

        shadow =
            if not (isInloop id session.net) then
                [ styleAttr "box-shadow" shadowStr ]

            else
                []

        sepDropAction =
            case session.actionMode of
                ProofMode { copyMode } ->
                    if
                        copyMode
                            == Deiteration
                            && (isDeiterationTarget dnd session id
                                    || existsAncestor (isDeiterationTarget dnd session) id session.net
                               )
                    then
                        []

                    else
                        List.map htmlAttribute <| DnD.droppable DragDropMsg Nothing

                _ ->
                    List.map htmlAttribute <| DnD.droppable DragDropMsg Nothing
    in
    column
        ([ width fill
         , height fill
         , Background.color (foregroundColor node.polarity)
         , sepBorderRadius
         ]
            ++ sepDropAction
            ++ shadow
        )
        [ outloopEl, inloopsEl ]


viewNodes : DnD -> Session -> Context -> List Tree -> Element Msg
viewNodes dnd session ctx trees =
    let
        nodeEl tree =
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
                ProofMode { copyMode } ->
                    if copyMode == Iteration then
                        case DnD.getDragId dnd of
                            Nothing ->
                                []

                            Just { source } ->
                                let
                                    (DragNode src) =
                                        source

                                    loc =
                                        { ctx = ctx, pos = List.length neighbors }

                                    iterateAction =
                                        Iterate src loc
                                in
                                case applicable iterateAction session of
                                    Err _ ->
                                        []

                                    Ok _ ->
                                        let
                                            dropStyle =
                                                droppableArea useColor

                                            dropTargetStyle =
                                                case DnD.getDropId dnd of
                                                    -- Hovering some droppable destination
                                                    Just (Just { destination }) ->
                                                        case destination of
                                                            DropLocation dropLoc ->
                                                                -- We only highlight when it's an (iterable) area
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
                                                    droppableArea reorderColor

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
            [ styleAttr "border-width" (String.fromInt (droppableArea Utils.Color.transparent).borderWidth ++ "px")
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
                        (nodeEl tree)

                nodesEls =
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
            wrappedRow attrs (nodesEls ++ addOTokenZone)

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
                        (nodeEl tree)

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


sessionHeightAttr : Attribute msg
sessionHeightAttr =
    styleAttr "height" ("calc(100vh - " ++ String.fromInt toolbarHeight ++ "px)")


viewSession : DnD -> Session -> Element Msg
viewSession dnd session =
    let
        sessionEl () =
            -- let
            --     _ =
            --         Debug.log "net" (stringOfNet session.net)
            -- in
            el
                ([ width fill
                 , sessionHeightAttr
                 , styleAttr "overflow-x" "hidden"
                 , styleAttr "overflow-y" "auto"
                 ]
                    ++ (List.map htmlAttribute <| DnD.droppable DragDropMsg Nothing)
                )
                (viewNodes dnd session TopLevel (hydrate session.net))
    in
    case session.actionMode of
        ProofMode _ ->
            sessionEl ()

        EditMode _ ->
            sessionEl ()

        _ ->
            el [ sessionHeightAttr, centerX ] (Widgets.fullPageTextMessage "Working on it!")
