module Model.Scroll exposing (..)

import Dict exposing (Dict)
import List.Extra
import Model.Formula as Formula exposing (..)
import Set
import Utils.List



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



-- Identifiers for nodes


type alias Id =
    ( List Int, Int )


baseId : Int -> Id
baseId i =
    ( [], i )


copyId : Id -> Id
copyId ( p, i ) =
    ( 0 :: p, i )


originalId : Id -> Id
originalId ( p, i ) =
    case p of
        0 :: p_ ->
            ( p_, i )

        _ ->
            ( p, i )



{- Whether `c == copyId o`. -}


isCopyId : Id -> Id -> Bool
isCopyId c o =
    (Tuple.first c == 0 :: Tuple.first o)
        && (Tuple.second c == Tuple.second o)


leftId : Id -> Id
leftId ( p, i ) =
    ( -1 :: p, i )


rightId : Id -> Id
rightId ( p, i ) =
    ( 1 :: p, i )



-- Scroll nets


type Origin
    = Direct Id
    | Indirect { anc : Id, src : Id }


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
justifyFrom copy justif =
    { justif | from = Just copy }


type alias Interaction =
    { opened : Bool
    , closed : Bool
    }


attachment : Interaction
attachment =
    { opened = False, closed = False }


open : Interaction -> Interaction
open interaction =
    { interaction | opened = True }


close : Interaction -> Interaction
close interaction =
    { interaction | closed = True }



{- `Sep ids int` encodes a **sep node** with children identified and ordered in `ids`, having a
   potential interaction with its parent `int`. That is, the sep is an *outloop* if `int = Nothing`,
   and an *inloop* if `int = Just ...`.
-}


type Shape
    = Formula Formula
    | Sep (List Id) (Maybe Interaction)


type Context
    = TopLevel
    | Inside Id


type alias Location =
    { ctx : Context, idx : Int }


type alias Node =
    { shape : Shape
    , name : String
    , justif : Justification
    , context : Context
    }


type alias Net =
    { nodes : Dict Id Node
    , roots : List Id
    }



-- Polarity


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



-- Query


getNode : Id -> Net -> Node
getNode id net =
    Dict.get id net.nodes
        |> Maybe.withDefault
            { shape =
                Formula
                    (Atom
                        (Image
                            { src = "node-not-found.png"
                            , description = "Node " ++ stringOfId id ++ " not found!"
                            }
                        )
                    )
            , name = "Error 404"
            , justif = assumption
            , context = TopLevel
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
    polarityOfContext (getContext id net) net


polarityOfContext : Context -> Net -> Polarity
polarityOfContext ctx net =
    case ctx of
        TopLevel ->
            Pos

        Inside parentId ->
            invert (getPolarity parentId net)


getInteractions : Id -> Net -> Dict Id Interaction
getInteractions id net =
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


getChildIds : Id -> Net -> List Id
getChildIds id net =
    case getShape id net of
        Sep childIds _ ->
            childIds

        _ ->
            []



{- Returns the singleton subnet of `net` rooted at `id`. -}


getSubnet : Id -> Net -> Net
getSubnet id =
    buildTree id >> dehydrateTree



-- Update


updateShapeNode : (Shape -> Shape) -> Node -> Node
updateShapeNode update node =
    { node | shape = update node.shape }


updateNameNode : (String -> String) -> Node -> Node
updateNameNode update node =
    { node | name = update node.name }


updateJustifNode : (Justification -> Justification) -> Node -> Node
updateJustifNode update node =
    { node | justif = update node.justif }


updateContextNode : (Context -> Context) -> Node -> Node
updateContextNode update node =
    { node | context = update node.context }


updateNode : Id -> (Node -> Node) -> Net -> Net
updateNode id update net =
    { net | nodes = Dict.update id (Maybe.map update) net.nodes }


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


freshIdContext : Context -> Id
freshIdContext context =
    case context of
        TopLevel ->
            ( [], 0 )

        Inside parentId ->
            Tuple.mapSecond (\x -> x + 1) parentId


freshId : Net -> Id
freshId net =
    let
        maxId nodeDict =
            Dict.foldl
                (\id _ acc ->
                    case id of
                        ( [], i ) ->
                            max i acc

                        _ ->
                            acc
                )
                -1
                nodeDict
    in
    ( [], maxId net.nodes + 1 )



{- Returns a net `t'` equal to `t` where all IDs have been changed so that they don't overlap with
   those in `s`.
-}


freshify : Net -> Net -> Net
freshify s t =
    idMapNet (Tuple.mapSecond (\i -> i + Tuple.second (freshId s))) t



-- Constructors and combinators
{- Updates the content of `t.nodes` with that of `s.nodes`, assuming the IDs of `s` form a subset of
   those of `t`. If this assumption is false, the result will be a non-sensical union with the roots
   of `t`.
-}


merge : Net -> Net -> Net
merge s t =
    { nodes = Dict.union s.nodes t.nodes
    , roots = t.roots
    }



-- Note that all operations other than `union` preserve uniqueness of identifiers


nodeOfShape : Shape -> Node
nodeOfShape shape =
    { shape = shape, name = "", justif = assumption, context = TopLevel }



{- The empty scroll net. -}


empty : Net
empty =
    { nodes = Dict.empty
    , roots = []
    }



{- The singleton net with formula `form`. -}


fo : Formula -> Net
fo form =
    let
        id =
            baseId 0
    in
    { nodes = Dict.fromList [ ( id, nodeOfShape (Formula form) ) ]
    , roots = [ id ]
    }



{- The juxtaposition `s ⊗ t` of nets `s` and `t`.

   This is implemented as the disjoint union `◅s ∪ ▻t`, where `◅s` is obtained by turning every `Id`
   `x` occurring in `s` (including references in `justif` fields) into `leftId x`, and symmetrically
   `▻t` is obtained by turning every `Id` `y` occurring in `t` into `rightId y`.
-}


juxtapose : Net -> Net -> Net
juxtapose s t =
    { nodes =
        Dict.union
            (idMapDict idMapNode leftId s.nodes)
            (idMapDict idMapNode rightId t.nodes)
    , roots =
        List.append
            (List.map leftId s.roots)
            (List.map rightId t.roots)
    }


juxtaposeList : List Net -> Net
juxtaposeList nets =
    List.foldl juxtapose empty nets



{- The curl (or n-ary scroll) `s ⫐ t1; ...; tn` with outloop `s` and inloops `t1`, ..., `tn`.

   This is implemented as the disjoint union `◅s ∪ ▻t1 ∪ ... ∪ ▻(...(▻tn))` where right injection is
   iterated `i` times for `ti`, to which we add appropriate `Sep`s for the outloop (ID `0`)
   and inloops (IDs `1` to `n`).
-}


curl : Net -> List Net -> Net
curl outloop inloops =
    { nodes =
        let
            outloopContent =
                idMapDict idMapNode leftId outloop.nodes

            inloopsContents =
                List.foldl
                    (\inloop ( acc, inj ) ->
                        let
                            inloopNodes =
                                idMapDict idMapNode inj inloop.nodes
                        in
                        ( Dict.union acc inloopNodes, inj >> rightId )
                    )
                    ( Dict.empty, rightId )
                    inloops
                    |> Tuple.first

            inloopsNodes =
                List.foldl
                    (\inloop ( acc, ( idx, inj ) ) ->
                        ( Dict.insert (baseId idx) (nodeOfShape (Sep inloop.roots (Just attachment))) acc
                        , ( idx + 1, inj >> rightId )
                        )
                    )
                    ( Dict.empty, ( 1, rightId ) )
                    inloops
                    |> Tuple.first

            outloopNode =
                let
                    childIds =
                        List.map leftId outloop.roots
                in
                Dict.insert (baseId 0) (nodeOfShape (Sep childIds Nothing)) Dict.empty
        in
        List.foldl Dict.union Dict.empty [ outloopContent, inloopsContents, outloopNode, inloopsNodes ]
    , roots =
        [ baseId 0 ]
    }



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
        dehydrateNodes (TNode node) acc =
            acc
                |> Dict.insert node.id node.node
                |> Dict.union (dehydrate node.children).nodes
    in
    { nodes = dehydrateNodes tree Dict.empty
    , roots = [ root.id ]
    }


dehydrate : List Tree -> Net
dehydrate trees =
    trees
        |> List.map dehydrateTree
        |> List.foldl juxtapose empty



-- Implementation note: ultimately it would be better to have tail recursive implementations,
-- although I do not expect too many levels of `Sep` nesting in actual programs


hydrateFormula : Context -> Formula -> Tree
hydrateFormula context form =
    TNode
        { id = freshIdContext context
        , node =
            { shape = Formula form
            , name = ""
            , justif = assumption
            , context = context
            }
        , children = []
        }


hydrateTokens : Context -> Bool -> List IToken -> Tree
hydrateTokens context isAttached tokens =
    let
        id =
            freshIdContext context

        children =
            List.map (hydrateIToken (Inside id)) tokens

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
            }
        , children = children
        }


hydrateOToken : Context -> OToken -> Tree
hydrateOToken context token =
    case token of
        OForm form ->
            hydrateFormula context form

        OSep tokens ->
            hydrateTokens context False tokens


hydrateIToken : Context -> IToken -> Tree
hydrateIToken context token =
    case token of
        ITok otoken ->
            hydrateOToken context otoken

        ISep tokens ->
            hydrateTokens context True tokens


hydrateStruct : Struct -> List Tree
hydrateStruct struct =
    List.map (hydrateOToken TopLevel) struct


netOfTokens : Context -> List IToken -> Net
netOfTokens context =
    List.map (hydrateIToken context) >> dehydrate


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
    List.foldl
        removeSingleNode
        (removeSingleNode id net)
        (getChildIds id net)



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
                                                        Sep (Utils.List.insert (loc.idx + i) [ srcNodeId ] childIds) int

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
                Utils.List.insert loc.idx srcFresh.roots tgt.roots

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
    net |> deeplink net |> idMapNet copyId



-- Illative transformations
{- Inserts token `tok` at location `loc` in `net`, by properly "dehydrating" it into a
   self-justified node with fresh IDs for its subnodes, as well as the correct attachment structure.
-}


insert : Location -> IToken -> Net -> Net
insert loc tok net =
    let
        newTree =
            hydrateIToken loc.ctx tok

        newNet =
            dehydrateTree newTree

        ins =
            updateJustif (getTreeId newTree) selfJustify newNet
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
    in
    graft loc copy net



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


isDestructed : Id -> Net -> Bool
isDestructed id net =
    isDestruction (getPolarity id net) (getJustif id net)


isExpanded : Id -> Net -> Bool
isExpanded id net =
    getInteractions id net
        |> Dict.toList
        |> Utils.List.exists (\( _, int ) -> isExpansion (getPolarity id net) int)


isCollapsed : Id -> Net -> Bool
isCollapsed id net =
    getInteractions id net
        |> Dict.toList
        |> Utils.List.exists (\( _, int ) -> isCollapse (getPolarity id net) int)



-- Detours


isDetour : Id -> Net -> Bool
isDetour id net =
    let
        ( pol, justif ) =
            ( getPolarity id net, getJustif id net )
    in
    (isCreation pol justif && isDestruction pol justif) || (isExpanded id net && isCollapsed id net)



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
            if isDestructed id net then
                prune id acc

            else
                acc
        )
        net
        net.nodes



-- Pretty-printing


stringOfId : Id -> String
stringOfId ( p, i ) =
    let
        prefix =
            p
                |> List.map String.fromInt
                |> List.intersperse "."
                |> List.foldl String.append ""
    in
    prefix ++ "::" ++ String.fromInt i


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
        ++ ":"
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
