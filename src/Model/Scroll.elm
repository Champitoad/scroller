module Model.Scroll exposing (..)

import Css exposing (text_)
import Dict exposing (Dict)
import List.Extra
import Model.Formula as Formula exposing (..)
import Utils.List
import Utils.Maybe



-- Scroll structures
{- Outer (unattached) token -}


type OToken
    = OForm Formula
    | OSep (List IToken)



{- Inner (possibly attached) token -}


type IToken
    = ITok OToken -- Unattached
    | ISep (List IToken) -- Attached



{- Generalized scroll structure -}


type alias Struct =
    List OToken


fo : Formula -> OToken
fo form =
    OForm form


a : String -> OToken
a name =
    fo (Formula.atom name)


curl : Struct -> List Struct -> OToken
curl outloop inloops =
    OSep
        ((outloop |> List.map ITok)
            ++ (inloops |> List.map (List.map ITok) |> List.map ISep)
        )


emptyScroll : OToken
emptyScroll =
    curl [] [ [] ]


identity : OToken
identity =
    curl [ a "a" ] [ [ a "a" ] ]


modusPonensCurryfied : OToken
modusPonensCurryfied =
    curl [ a "a", curl [ a "a" ] [ [ a "b" ] ] ] [ [ a "b" ] ]


orElim : OToken
orElim =
    curl [ curl [] [ [ a "a" ], [ a "b" ] ], curl [ a "a" ] [ [ a "c" ] ], curl [ a "b" ] [ [ a "c" ] ] ] [ [ a "c" ] ]



{- Interpreting a formula into the corresponding scroll structure -}


interpretFormula : Formula -> Struct
interpretFormula form =
    case form of
        Atom _ ->
            [ OForm form ]

        Truth ->
            []

        Falsity ->
            [ OSep [] ]

        And f1 f2 ->
            interpretFormula f1 ++ interpretFormula f2

        Or f1 f2 ->
            [ OSep
                [ ISep (f1 |> interpretFormula |> List.map ITok)
                , ISep (f2 |> interpretFormula |> List.map ITok)
                ]
            ]

        Implies f1 f2 ->
            [ OSep
                ((f1 |> interpretFormula |> List.map ITok)
                    ++ [ ISep (f2 |> interpretFormula |> List.map ITok) ]
                )
            ]

        Not f1 ->
            [ OSep (f1 |> interpretFormula |> List.map ITok) ]



-- Identifiers for nodes


type alias Id =
    Int



-- Scroll nets


type Origin
    = Direct Id
    | Indirect { anc : Id, src : Id }


originSourceId : Origin -> Id
originSourceId origin =
    case origin of
        Direct id ->
            id

        Indirect { src } ->
            src


type alias Justification =
    { self : Bool
    , from : Maybe Origin
    }


assumption : Justification
assumption =
    { self = False, from = Nothing }


selfJustify : Justification -> Justification
selfJustify justif =
    { justif | self = True }


justifyFrom : Origin -> Justification -> Justification
justifyFrom origin justif =
    { justif | from = Just origin }


type alias Interaction =
    { opened : Bool
    , closed : Bool
    }


attachment : Interaction
attachment =
    { opened = False, closed = False }


makeOpened : Interaction -> Interaction
makeOpened interaction =
    { interaction | opened = True }


makeClosed : Interaction -> Interaction
makeClosed interaction =
    { interaction | closed = True }



{- `Sep ids int` encodes a **sep node** with children identified and ordered in `ids`, having a
   potential interaction `int` with its parent. That is, the sep is an *outloop* if `int = Nothing`,
   and an *inloop* if `int = Just ...`.
-}


type Shape
    = Formula Formula
    | Sep (List Id) (Maybe Interaction)


type Context
    = TopLevel
    | Inside Id


type alias Location =
    { ctx : Context, pos : Int }


type Polarity
    = Pos
    | Neg


invert : Polarity -> Polarity
invert polarity =
    case polarity of
        Pos ->
            Neg

        Neg ->
            Pos


type alias Node =
    { shape : Shape
    , name : String
    , justif : Justification
    , context : Context
    , polarity : Polarity
    }


type alias Net =
    { nodes : Dict Id Node
    , roots : List Id
    }


empty : Net
empty =
    { nodes = Dict.empty
    , roots = []
    }


juxtapose : Net -> Net -> Net
juxtapose s t =
    let
        t_ =
            freshify s t
    in
    { nodes = Dict.union s.nodes t_.nodes
    , roots = List.append s.roots t_.roots
    }


juxtaposeList : List Net -> Net
juxtaposeList nets =
    List.foldr juxtapose empty nets



-- Query


findAncestor : (Id -> Net -> Bool) -> Id -> Net -> Maybe Id
findAncestor pred id net =
    case getContext id net of
        TopLevel ->
            Nothing

        Inside parentId ->
            if pred parentId net then
                Just parentId

            else
                findAncestor pred parentId net


foldAncestors : (Id -> Net -> a -> a) -> a -> Id -> Net -> a
foldAncestors func acc id net =
    case getContext id net of
        TopLevel ->
            acc

        Inside parentId ->
            foldAncestors func (func parentId net acc) parentId net


existsAncestor : (Id -> Net -> Bool) -> Id -> Net -> Bool
existsAncestor pred id net =
    Utils.Maybe.isSomething (findAncestor pred id net)


existsAncestorContext : (Id -> Net -> Bool) -> Context -> Net -> Bool
existsAncestorContext pred ctx net =
    case ctx of
        TopLevel ->
            False

        Inside parentId ->
            pred parentId net
                || existsAncestor pred parentId net


getNode : Id -> Net -> Node
getNode id net =
    Dict.get id net.nodes
        |> Maybe.withDefault
            { shape =
                Formula
                    (Atom
                        (Image
                            { src = "../../public/assets/img/node-not-found.png"
                            , description = "Node " ++ stringOfId id ++ " not found!"
                            }
                        )
                    )
            , name = "Error 404"
            , justif = assumption
            , context = TopLevel
            , polarity = Pos
            }


getShape : Id -> Net -> Shape
getShape id net =
    (getNode id net).shape


getName : Id -> Net -> String
getName id net =
    (getNode id net).name


getJustif : Id -> Net -> Justification
getJustif id net =
    (getNode id net).justif


getContext : Id -> Net -> Context
getContext id net =
    (getNode id net).context


getPolarity : Id -> Net -> Polarity
getPolarity id net =
    -- foldAncestors (\_ _ -> invert) Pos
    (getNode id net).polarity


getPolarityContext : Context -> Net -> Polarity
getPolarityContext ctx net =
    case ctx of
        TopLevel ->
            Pos

        Inside parentId ->
            invert (getPolarity parentId net)


getLocation : Id -> Net -> Location
getLocation id net =
    case getContext id net of
        TopLevel ->
            { ctx = TopLevel
            , pos = Maybe.withDefault -1 (List.Extra.elemIndex id net.roots)
            }

        Inside parentId ->
            { ctx = Inside parentId
            , pos = Maybe.withDefault -1 (List.Extra.elemIndex id (getChildIds parentId net))
            }


getNodeIdAtLocation : Location -> Net -> Id
getNodeIdAtLocation loc net =
    net.nodes
        |> Dict.toList
        |> List.Extra.findMap
            (\( id, _ ) ->
                if getLocation id net == loc then
                    Just id

                else
                    Nothing
            )
        -- Dummy ID, should never happen
        |> Maybe.withDefault -1


getOutloopInteractions : Id -> Net -> Dict Id Interaction
getOutloopInteractions id net =
    case getShape id net of
        Sep childIds _ ->
            List.foldl
                (\childId acc ->
                    case getShape childId net of
                        Sep _ (Just interaction) ->
                            Dict.insert childId interaction acc

                        _ ->
                            Dict.empty
                )
                Dict.empty
                childIds

        _ ->
            Dict.empty


getInloopInteraction : Id -> Net -> Maybe Interaction
getInloopInteraction id net =
    case getShape id net of
        Sep _ interaction ->
            interaction

        _ ->
            Nothing


getChildIds : Id -> Net -> List Id
getChildIds id net =
    case getShape id net of
        Sep childIds _ ->
            childIds

        _ ->
            []


getChildIdsContext : Context -> Net -> List Id
getChildIdsContext ctx net =
    case ctx of
        TopLevel ->
            net.roots

        Inside parentId ->
            getChildIds parentId net


getPosition : Id -> Net -> Int
getPosition id net =
    let
        neighbors =
            getChildIdsContext (getContext id net) net
    in
    List.Extra.elemIndex id neighbors
        -- Dummy position, should never happen
        |> Maybe.withDefault -1



{- Reflexive-transitive closure of `getChildIds`. -}


getDescendentIds : Id -> Net -> List Id
getDescendentIds id net =
    id :: List.concatMap (\cid -> getDescendentIds cid net) (getChildIds id net)



{- Returns the singleton subnet of `net` rooted at `id`. -}


getSubnet : Id -> Net -> Net
getSubnet id =
    buildTree id >> dehydrateTree



{- Returns the subnet of `net` whose roots are the (direct) children in context `ctx`. -}


getSubnetContext : Context -> Net -> Net
getSubnetContext ctx net =
    case ctx of
        TopLevel ->
            net

        Inside parentId ->
            let
                descendentIds =
                    getDescendentIds parentId net
            in
            { nodes =
                net.nodes
                    |> Dict.remove parentId
                    |> Dict.filter (\id _ -> List.member id descendentIds)
            , roots =
                getChildIds parentId net
            }


isOutloop : Id -> Net -> Bool
isOutloop id net =
    case getShape id net of
        Sep _ Nothing ->
            True

        _ ->
            False


isInloop : Id -> Net -> Bool
isInloop id net =
    case getShape id net of
        Sep _ (Just _) ->
            True

        _ ->
            False


getOutloop : Id -> Net -> List Tree
getOutloop id net =
    net
        |> buildTree id
        |> getTreeChildren
        |> List.filter (\(TNode root) -> not (isInloop root.id net))



{- Whether `src` spans `dst`, meaning that every node in `dst` is in scope of those in `src`. -}


spans : Context -> Context -> Net -> Bool
spans src dst net =
    case ( src, dst ) of
        ( TopLevel, _ ) ->
            True

        ( _, TopLevel ) ->
            True

        ( Inside srcId, Inside dstId ) ->
            -- `dst` is equal to, or inside `src`
            List.member dstId (getDescendentIds srcId net)



-- Update


updateInteractionShape : (Interaction -> Interaction) -> Shape -> Shape
updateInteractionShape update shape =
    case shape of
        Sep childIds (Just interaction) ->
            Sep childIds (Just (update interaction))

        _ ->
            shape


updateShapeNode : (Shape -> Shape) -> Node -> Node
updateShapeNode update node =
    { node | shape = update node.shape }


updateNameNode : (String -> String) -> Node -> Node
updateNameNode update node =
    { node | name = update node.name }


updateJustifNode : (Justification -> Justification) -> Node -> Node
updateJustifNode update node =
    { node | justif = update node.justif }


updateInteractionNode : (Interaction -> Interaction) -> Node -> Node
updateInteractionNode update node =
    updateShapeNode (updateInteractionShape update) node


updateContextNode : (Context -> Context) -> Node -> Node
updateContextNode update node =
    { node | context = update node.context }


updateNode : Id -> (Node -> Node) -> Net -> Net
updateNode id update net =
    { net | nodes = Dict.update id (Maybe.map update) net.nodes }


updateInteraction : Id -> (Interaction -> Interaction) -> Net -> Net
updateInteraction id update net =
    updateShape id (updateInteractionShape update) net


updateShape : Id -> (Shape -> Shape) -> Net -> Net
updateShape id update net =
    updateNode id (updateShapeNode update) net


updateName : Id -> (String -> String) -> Net -> Net
updateName id update net =
    updateNode id (updateNameNode update) net


updateContext : Id -> (Context -> Context) -> Net -> Net
updateContext id update net =
    updateNode id (updateContextNode update) net


updateJustif : Id -> (Justification -> Justification) -> Net -> Net
updateJustif id update net =
    updateNode id (updateJustifNode update) net



-- Identifier handling


idMapShape : (Id -> Id) -> Shape -> Shape
idMapShape f shape =
    case shape of
        Formula _ ->
            shape

        Sep childIds interaction ->
            Sep (List.map f childIds) interaction


idMapOrigin : (Id -> Id) -> Origin -> Origin
idMapOrigin f copy =
    case copy of
        Direct src ->
            Direct (f src)

        Indirect j ->
            Indirect { j | anc = f j.anc, src = f j.src }


idMapJustif : (Id -> Id) -> Justification -> Justification
idMapJustif f justif =
    { justif
        | from = Maybe.map (idMapOrigin f) justif.from
    }


idMapContext : (Id -> Id) -> Context -> Context
idMapContext f ctx =
    case ctx of
        TopLevel ->
            TopLevel

        Inside parentId ->
            Inside (f parentId)


idMapNode : (Id -> Id) -> Node -> Node
idMapNode f node =
    { node
        | shape = idMapShape f node.shape
        , justif = idMapJustif f node.justif
        , context = idMapContext f node.context
    }


idMapDict : ((Id -> Id) -> a -> a) -> (Id -> Id) -> Dict Id a -> Dict Id a
idMapDict vmap f dict =
    dict
        |> Dict.toList
        |> List.map (\( id, v ) -> ( f id, vmap f v ))
        |> Dict.fromList


idMapNet : (Id -> Id) -> Net -> Net
idMapNet f net =
    { nodes = idMapDict idMapNode f net.nodes
    , roots = List.map f net.roots
    }


freshId : Net -> Id
freshId net =
    let
        maxId nodeDict =
            nodeDict |> Dict.keys |> List.foldl max -1
    in
    maxId net.nodes + 1



{- Returns a net `t'` equal to `t` where all IDs have been changed so that they don't overlap with
   those in `s`.
-}


freshify : Net -> Net -> Net
freshify s t =
    let
        fId =
            freshId s
    in
    idMapNet (\id -> id + fId) t



-- Hydration pattern


type Tree
    = TNode
        { id : Id
        , node : Node
        , children : List Tree
        }


getTreeId : Tree -> Id
getTreeId (TNode { id }) =
    id


getTreeNode : Tree -> Node
getTreeNode (TNode { node }) =
    node


getTreeChildren : Tree -> List Tree
getTreeChildren (TNode { children }) =
    children


hydrate : Net -> List Tree
hydrate net =
    net.roots
        |> List.map (\id -> buildTree id net)


buildTree : Id -> Net -> Tree
buildTree id net =
    let
        node =
            getNode id net

        resolvedChildren =
            case node.shape of
                Formula _ ->
                    []

                Sep childIds _ ->
                    childIds
                        |> List.map (\cid -> buildTree cid net)
    in
    TNode
        { id = id
        , node = node
        , children = resolvedChildren
        }


dehydrateTree : Tree -> Net
dehydrateTree ((TNode root) as tree) =
    let
        dehydrateNodes acc (TNode node) =
            acc
                |> Dict.insert node.id node.node
                |> Dict.union
                    (node.children
                        |> List.map (dehydrateNodes Dict.empty)
                        |> List.foldl Dict.union Dict.empty
                    )
    in
    { nodes = dehydrateNodes Dict.empty tree
    , roots = [ root.id ]
    }


dehydrate : List Tree -> Net
dehydrate trees =
    trees |> List.map dehydrateTree |> juxtaposeList



-- Implementation note: ultimately it would be better to have tail recursive implementations,
-- although I do not expect too many levels of `Sep` nesting in actual nets.


hydrateFormula : Id -> Context -> Polarity -> Formula -> Tree
hydrateFormula id context polarity form =
    TNode
        { id = id
        , node =
            { shape = Formula form
            , name = ""
            , justif = assumption
            , context = context
            , polarity = polarity
            }
        , children = []
        }


hydrateITokens : Id -> Context -> Polarity -> Bool -> List IToken -> Tree
hydrateITokens id context polarity isAttached tokens =
    let
        children =
            List.indexedMap (\i tok -> hydrateIToken (id + i + 1) (Inside id) (invert polarity) tok) tokens

        interaction =
            if isAttached then
                Just attachment

            else
                Nothing
    in
    TNode
        { id = id
        , node =
            { shape = Sep (List.map getTreeId children) interaction
            , name = ""
            , justif = assumption
            , context = context
            , polarity = polarity
            }
        , children = children
        }


hydrateOToken : Id -> Context -> Polarity -> OToken -> Tree
hydrateOToken id context polarity token =
    case token of
        OForm form ->
            hydrateFormula id context polarity form

        OSep tokens ->
            hydrateITokens id context polarity False tokens


hydrateIToken : Id -> Context -> Polarity -> IToken -> Tree
hydrateIToken id context polarity token =
    case token of
        ITok otoken ->
            hydrateOToken id context polarity otoken

        ISep tokens ->
            hydrateITokens id context polarity True tokens


hydrateStruct : Struct -> List Tree
hydrateStruct struct =
    List.indexedMap (\i tok -> hydrateOToken i TopLevel Pos tok) struct


netOfITokens : Context -> Polarity -> List IToken -> Net
netOfITokens context polarity =
    List.indexedMap (\i tok -> hydrateIToken i context polarity tok) >> dehydrate


netOfStruct : Struct -> Net
netOfStruct =
    hydrateStruct >> dehydrate



-- Surgery


removeSingleNode : Id -> Net -> Net
removeSingleNode id net =
    { nodes = Dict.remove id net.nodes
    , roots = List.Extra.remove id net.roots
    }



{- Prunes the subnet with root `id` in `net`. -}


prune : Id -> Net -> Net
prune id net =
    let
        removeIdFromSep shape =
            case shape of
                Sep childIds int ->
                    Sep (List.Extra.remove id childIds) int

                _ ->
                    shape

        withoutParent =
            case getContext id net of
                TopLevel ->
                    net

                Inside parentId ->
                    updateShape parentId removeIdFromSep net
    in
    List.foldl removeSingleNode withoutParent (getDescendentIds id net)



{- Grafts net `src` onto net `tgt` at location `loc`. -}


graft : Location -> Net -> Net -> Net
graft loc src tgt =
    let
        srcFresh =
            freshify tgt src
    in
    { nodes =
        Dict.foldl
            (\srcNodeId srcNode acc ->
                let
                    ( updatedSrcNode, srcRootIdx ) =
                        case List.Extra.elemIndex srcNodeId srcFresh.roots of
                            Just i ->
                                ( { srcNode | context = loc.ctx }, Just i )

                            Nothing ->
                                ( srcNode, Nothing )

                    accWithSrcNode =
                        Dict.insert srcNodeId updatedSrcNode acc
                in
                case loc.ctx of
                    -- If we graft at the top-level, just add the node
                    TopLevel ->
                        accWithSrcNode

                    -- If we graft in a sep and the node is a root,
                    -- then we need to link the sep to its new child
                    Inside parentId ->
                        accWithSrcNode
                            |> Dict.update parentId
                                (Maybe.map
                                    (\parent ->
                                        let
                                            updatedShape =
                                                case ( parent.shape, srcRootIdx ) of
                                                    --
                                                    ( Sep childIds int, Just i ) ->
                                                        Sep (Utils.List.insert (loc.pos + i) [ srcNodeId ] childIds) int

                                                    ( shape, _ ) ->
                                                        shape
                                        in
                                        { parent | shape = updatedShape }
                                    )
                                )
            )
            tgt.nodes
            srcFresh.nodes
    , roots =
        case loc.ctx of
            TopLevel ->
                Utils.List.insert loc.pos srcFresh.roots tgt.roots

            _ ->
                tgt.roots
    }



{- Assuming that `source` and `target` are isomorphic, this updates the `justif.from` field of every
   node in `target` so that it holds a `Copy` backpointer to the corresponding node in `source`. The
   root node is a `Direct` copy, while subnodes are `Indirect` copies.
-}


deeplinkTree : Tree -> Tree -> Tree
deeplinkTree source target =
    let
        aux ancId src tgt =
            let
                srcId =
                    getTreeId src

                tgtNode =
                    getTreeNode tgt

                tgtJustif =
                    tgtNode.justif

                ( copy, acc ) =
                    case ancId of
                        Nothing ->
                            ( Direct srcId, Just srcId )

                        Just id ->
                            ( Indirect { anc = id, src = srcId }, Just id )
            in
            TNode
                { id = getTreeId tgt
                , node = { tgtNode | justif = { tgtJustif | from = Just copy } }
                , children = List.map2 (aux acc) (getTreeChildren src) (getTreeChildren tgt)
                }
    in
    aux Nothing source target


deeplink : Net -> Net -> Net
deeplink source target =
    dehydrate (List.map2 deeplinkTree (hydrate source) (hydrate target))



{- Returns a deep copy of `net` with fresh IDs, holding `Copy` backpointers to the original nodes. -}


deepcopy : Net -> Net
deepcopy net =
    net |> deeplink net |> freshify net



-- Illative transformations
{- Generates a fresh name that does not appear in `net` from `basename`. -}


freshName : String -> Net -> String
freshName basename net =
    let
        generateName str num =
            str ++ " " ++ String.fromInt num

        number =
            Dict.foldl
                (\_ { name } acc ->
                    if name == generateName basename acc then
                        acc + 1

                    else
                        acc
                )
                0
                net.nodes
    in
    generateName basename number



{- Inserts token `tok` at location `loc` in `net`, by properly "dehydrating" it into a
   (self-justified if `self == True`) node with fresh IDs for its subnodes, as well as the correct
   attachment structure.
-}


insert : Bool -> Location -> IToken -> Net -> Net
insert self loc tok net =
    let
        newTree =
            hydrateIToken 0 loc.ctx (getPolarityContext loc.ctx net) tok

        newId =
            getTreeId newTree

        basename =
            case tok of
                ITok _ ->
                    "Param"

                ISep _ ->
                    "Branch"

        ins =
            dehydrateTree newTree
                |> updateName newId (\_ -> freshName basename net)
                |> updateJustif newId
                    (\j ->
                        if self then
                            selfJustify j

                        else
                            j
                    )
    in
    graft loc ins net



{- "Deletes" node `id` in `net` by marking it as self-justified. -}


delete : Id -> Net -> Net
delete id net =
    updateJustif id selfJustify net



{- Iterates node `id` at location `loc` in `net` by inserting a deep copy of the conclusion of `id`. -}


iterate : Id -> Location -> Net -> Net
iterate id loc net =
    let
        copy =
            net
                |> getSubnet id
                |> conclusion
                |> deepcopy
                |> updateName id (\name -> freshName ("Copy of " ++ name) net)
    in
    graft loc copy net



{- Updates the content of `t.nodes` with that of `s.nodes`, assuming the IDs of `s` form a subset of
   those of `t`. If this assumption is false, the result will be a non-sensical union with the roots
   of `t`.
-}


merge : Net -> Net -> Net
merge s t =
    { nodes = Dict.union s.nodes t.nodes
    , roots = t.roots
    }



{- "Deiterates" node `tgt` from `src` in `net` by deep linking the conclusion of `tgt` back to that of `src`. -}


deiterate : Id -> Id -> Net -> Net
deiterate src tgt net =
    let
        ( subnetSrc, subnetTgt ) =
            ( getSubnet src net, getSubnet tgt net )

        backlinkedTgtConclusion =
            deeplink (conclusion subnetSrc) (conclusion subnetTgt)
    in
    merge backlinkedTgtConclusion net



-- Querying edit status


isInsertion : Polarity -> Justification -> Bool
isInsertion polarity justif =
    polarity == Neg && justif.self


isDeletion : Polarity -> Justification -> Bool
isDeletion polarity justif =
    polarity == Pos && justif.self


isDirect : Justification -> Bool
isDirect justif =
    case justif.from of
        Just (Direct _) ->
            True

        _ ->
            False


isIteration : Polarity -> Justification -> Bool
isIteration polarity justif =
    polarity == Pos && isDirect justif


isDeiteration : Polarity -> Justification -> Bool
isDeiteration polarity justif =
    polarity == Neg && isDirect justif


isExpansion : Polarity -> Interaction -> Bool
isExpansion pol int =
    (pol == Pos && int.opened) || (pol == Neg && int.closed)


isCollapse : Polarity -> Interaction -> Bool
isCollapse pol int =
    (pol == Pos && int.closed) || (pol == Neg && int.opened)


isCreation : Polarity -> Justification -> Bool
isCreation polarity justif =
    isInsertion polarity justif || isIteration polarity justif


isDestruction : Polarity -> Justification -> Bool
isDestruction polarity justif =
    isDeletion polarity justif || isDeiteration polarity justif


isCreated : Id -> Net -> Bool
isCreated id net =
    isCreation (getPolarity id net) (getJustif id net)


isDestroyed : Id -> Net -> Bool
isDestroyed id net =
    isDestruction (getPolarity id net) (getJustif id net)


isExpandedOutloop : Id -> Net -> Bool
isExpandedOutloop id net =
    getOutloopInteractions id net
        |> Dict.toList
        |> Utils.List.exists (\( _, int ) -> isExpansion (getPolarity id net) int)


isCollapsedOutloop : Id -> Net -> Bool
isCollapsedOutloop id net =
    getOutloopInteractions id net
        |> Dict.toList
        |> Utils.List.exists (\( _, int ) -> isCollapse (getPolarity id net) int)


isExpandedInloop : Id -> Net -> Bool
isExpandedInloop id net =
    getInloopInteraction id net
        |> Maybe.map (isExpansion (getPolarity id net))
        |> Maybe.withDefault False


isCollapsedInloop : Id -> Net -> Bool
isCollapsedInloop id net =
    getInloopInteraction id net
        |> Maybe.map (isCollapse (getPolarity id net))
        |> Maybe.withDefault False


isIntroduced : Id -> Net -> Bool
isIntroduced id net =
    isCreated id net
        || isExpandedOutloop id net
        || isExpandedInloop id net
        || existsAncestor isCreated id net


isIntroducedContext : Context -> Net -> Bool
isIntroducedContext ctx net =
    case ctx of
        TopLevel ->
            False

        Inside parentId ->
            isIntroduced parentId net


isEliminated : Id -> Net -> Bool
isEliminated id net =
    isDestroyed id net
        || isCollapsedOutloop id net
        || isCollapsedInloop id net
        || existsAncestor isDestroyed id net


isEliminatedContext : Context -> Net -> Bool
isEliminatedContext ctx net =
    case ctx of
        TopLevel ->
            False

        Inside parentId ->
            isEliminated parentId net



-- Detours


isDetour : Id -> Net -> Bool
isDetour id net =
    let
        ( pol, justif ) =
            ( getPolarity id net, getJustif id net )
    in
    (isCreation pol justif && isDestruction pol justif) || (isExpandedOutloop id net && isCollapsedOutloop id net)



{- A net is normal when it is detour-free -}


isNormal : Net -> Bool
isNormal net =
    net.nodes
        |> Dict.keys
        |> Utils.List.forall (\id -> not (isDetour id net))



-- Boundaries


premiss : Net -> Net
premiss net =
    Dict.foldl
        (\id _ acc ->
            if isCreated id net then
                prune id acc

            else
                acc
        )
        net
        net.nodes


conclusion : Net -> Net
conclusion net =
    Dict.foldl
        (\id _ acc ->
            if isDestroyed id net then
                prune id acc

            else
                acc
        )
        net
        net.nodes


tokenOfTree : Tree -> IToken
tokenOfTree (TNode root) =
    case root.node.shape of
        Formula form ->
            ITok (OForm form)

        Sep _ interaction ->
            let
                childrenTokens =
                    List.map tokenOfTree root.children
            in
            case interaction of
                Nothing ->
                    ITok (OSep childrenTokens)

                Just _ ->
                    ISep childrenTokens


structOfNet : Net -> Struct
structOfNet =
    hydrate
        >> List.filterMap
            (\tree ->
                case tokenOfTree tree of
                    ITok otoken ->
                        Just otoken

                    _ ->
                        Nothing
            )


premissStruct : Net -> Struct
premissStruct =
    premiss >> structOfNet


conclusionStruct : Net -> Struct
conclusionStruct =
    conclusion >> structOfNet



-- Pretty-printing


stringOfId : Id -> String
stringOfId =
    String.fromInt


stringOfNet : Net -> String
stringOfNet net =
    net
        |> hydrate
        >> stringOfTreeList net


stringOfTree : Net -> Tree -> String
stringOfTree net (TNode { node, children }) =
    children
        |> stringOfTreeList net
        |> (\content -> stringOfNode net content node)


stringOfTreeList : Net -> List Tree -> String
stringOfTreeList net trees =
    trees
        |> List.map (stringOfTree net)
        >> List.intersperse ", "
        >> List.foldl String.append ""


stringOfNode : Net -> String -> Node -> String
stringOfNode net content node =
    stringOfJustification net node.justif
        ++ node.name
        ++ (if String.isEmpty node.name then
                ""

            else
                ":"
           )
        ++ stringOfShape content node.shape


stringOfCopy : Net -> Origin -> String
stringOfCopy net copy =
    case copy of
        Direct srcId ->
            getName srcId net

        _ ->
            ""


stringOfJustification : Net -> Justification -> String
stringOfJustification net { self, from } =
    let
        selfText =
            if self then
                "•"

            else
                ""

        fromText =
            case from of
                Just copy ->
                    stringOfCopy net copy ++ "/"

                Nothing ->
                    ""
    in
    fromText ++ selfText


stringOfShape : String -> Shape -> String
stringOfShape content shape =
    case shape of
        Formula form ->
            Formula.toString form

        Sep _ interaction ->
            case interaction of
                Nothing ->
                    "[" ++ content ++ "]"

                Just _ ->
                    "(" ++ content ++ ")"
