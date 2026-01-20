module Model.Scroll exposing (..)

import Dict exposing (Dict)
import Model.Formula exposing (..)
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


leftId : Id -> Id
leftId ( p, i ) =
    ( 0 :: p, i )


rightId : Id -> Id
rightId ( p, i ) =
    ( 1 :: p, i )



-- Scroll nets


type Copy
    = Direct Id
    | Indirect { anc : Id, src : Id }


type alias Justification =
    { self : Bool
    , from : Maybe Copy
    }


assumption : Justification
assumption =
    { self = False, from = Nothing }


type alias Interaction =
    { opened : Bool
    , closed : Bool
    }


attachment : Interaction
attachment =
    { opened = False, closed = False }


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
        Sep children _ ->
            List.foldl
                (\childId acc ->
                    case getShape childId net of
                        Sep _ (Just interaction) ->
                            Dict.insert childId interaction acc

                        _ ->
                            Dict.empty
                )
                Dict.empty
                children

        _ ->
            Dict.empty



-- Identifier handling


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


idMapShape : (Id -> Id) -> Shape -> Shape
idMapShape f shape =
    case shape of
        Formula _ ->
            shape

        Sep children interaction ->
            Sep (List.map f children) interaction


idMapCopy : (Id -> Id) -> Copy -> Copy
idMapCopy f copy =
    case copy of
        Direct src ->
            Direct (f src)

        Indirect j ->
            Indirect { j | anc = f j.anc, src = f j.src }


idMapJustif : (Id -> Id) -> Justification -> Justification
idMapJustif f justif =
    { justif
        | from = Maybe.map (idMapCopy f) justif.from
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
-- Note that all operations here preserve uniqueness of identifiers


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



{- Grafts net `src` onto net `tgt` at location `loc`. -}


graft : Location -> Net -> Net -> Net
graft { ctx, idx } src tgt =
    { nodes =
        Dict.foldl
            (\srcNodeId srcNode acc ->
                let
                    updatedSrcNode =
                        -- if List.mem srcNodeId
                        Debug.todo ""
                in
                case ctx of
                    TopLevel ->
                        -- Dict.insert srcNodeId { srcNode acc
                        Debug.todo ""

                    Inside parentId ->
                        Debug.todo ""
            )
            tgt.nodes
            (freshify tgt src).nodes
    , roots =
        Debug.todo ""
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
                    children =
                        List.map leftId outloop.roots
                in
                Dict.insert (baseId 0) (nodeOfShape (Sep children Nothing)) Dict.empty
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

                Sep children _ ->
                    children
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
    , roots = List.map getTreeId root.children
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



-- Surgery
{- `prune net id` removes the subnet with root `id` in `net`. -}


prune : Id -> Net -> Net
prune id net =
    Debug.todo ""



{- `insert net loc tok` inserts token `tok` at location `loc` in `net`, by properly "dehydrating" it
   into a node with fresh IDs for every subnodes, as well as the correct attachment structure.
-}


insert : Location -> IToken -> Net -> Net
insert { ctx, idx } tok net =
    let
        id =
            freshId net

        node =
            { shape =
                Debug.todo ""
            , name =
                Debug.todo ""
            , justif =
                assumption
            , parent =
                ctx
            , polarity =
                polarityOfContext ctx net
            }

        acc =
            { nodes =
                Dict.insert id net
            , roots =
                case ctx of
                    TopLevel ->
                        Utils.List.insert idx id net.roots

                    _ ->
                        net.roots
            }
    in
    Debug.todo ""



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


premiss : Net -> Struct
premiss net =
    Debug.todo ""
