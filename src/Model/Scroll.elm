module Model.Scroll exposing (..)

import Dict exposing (Dict)
import Model.Formula exposing (..)
import Utils.List
import Utils.Maybe



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


type alias Interaction =
    { opened : Bool
    , closed : Bool
    }


type Shape
    = Form Formula
    | Sep (List ( Id, Maybe Interaction ))


type Context
    = TopLevel
    | Inside Id


type alias Location =
    { ctx : Context, idx : Int }


type alias Node =
    { shape : Shape
    , name : String
    , justif : Justification

    -- The following fields are used for memoization and efficient access/update:
    , parent : Context
    , polarity : Polarity
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


getNode : Net -> Id -> Node
getNode net id =
    Dict.get id net.nodes
        |> Maybe.withDefault
            { shape = Form (Atom (Image { src = "node-not-found.png", description = "Node not found!" }))
            , name = "Error 404"
            , parent = TopLevel
            , polarity = Pos
            , justif = assumption
            }


getInteractions : Net -> Id -> Dict Id Interaction
getInteractions net id =
    case (getNode net id).shape of
        Sep children ->
            children
                |> List.filterMap (\( cid, int ) -> Maybe.map (\x -> ( cid, x )) int)
                |> Dict.fromList

        _ ->
            Dict.empty


getShape : Net -> Id -> Shape
getShape net id =
    (getNode net id).shape


getName : Net -> Id -> String
getName net id =
    (getNode net id).name


getJustif : Net -> Id -> Justification
getJustif net id =
    (getNode net id).justif


getParent : Net -> Id -> Context
getParent net id =
    (getNode net id).parent


getPolarity : Net -> Id -> Polarity
getPolarity net id =
    (getNode net id).polarity


polarityOfContext : Net -> Context -> Polarity
polarityOfContext net ctx =
    case ctx of
        TopLevel ->
            Pos

        Inside sep ->
            invert (getPolarity net sep)



-- Constructors


assumption : Justification
assumption =
    { self = False, from = Nothing }


attachment : Interaction
attachment =
    { opened = False, closed = False }


nodeOfShape : Shape -> Node
nodeOfShape shape =
    { shape = shape, name = "", justif = assumption, parent = TopLevel, polarity = Pos }



{- The empty scroll net -}


empty : Net
empty =
    { nodes = Dict.empty
    , roots = []
    }



{- `fo form` returns the singleton net with formula `form` -}


fo : Formula -> Net
fo form =
    { nodes = Dict.fromList [ ( baseId 0, nodeOfShape (Form form) ) ]
    , roots = [ baseId 0 ]
    }



{- `juxtapose s t` returns the juxtaposition `s ⊗ t` of the nets `s` and `t`.

   This involves performing the following operations:
   - `nodes` is the disjoint union `◅s ∪ ▻t`, where `◅s` is obtained by turning every `Id`
     `x` occurring in `s` (including references in `justif` fields) into `leftId x`, and
     symmetrically `▻t` is obtained by turning every `Id` `y` occurring in `t` into `rightId y`
   - `roots` is the disjoint concatenation of `s.roots` and `t.roots`
   - `interactions` is the disjoint union of `s.interactions` and `t.interactions`
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



{- `curl s [t1, ..., tn]` returns the curl (or n-ary scroll) `s ⫐ t1; ...; tn` with outloop `s` and
   inloops `t1`, ..., `tn`.

   This involves performing the following operations:
   - `nodes` is the disjoint union `◅s ∪ ▻t1 ∪ ... ∪ ▻(...(▻tn))` where right injection is iterated
     `i` times for `ti.nodes`, to which we add appropriate `Sep`s for the outloop (ID `0`) and
     inloops (IDs `1` to `n`)
   - `roots` is the singleton array `[0]`
   - `interactions` is the disjoint union of the interactions of the outloop and inloops, to which
     we add attachments `(0, 1)` ... `(0, n)`
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
                        let
                            children =
                                List.map (\cid -> ( inj cid, Nothing )) inloop.roots
                        in
                        ( Dict.insert (baseId idx) (nodeOfShape (Sep children)) acc
                        , ( idx + 1, inj >> rightId )
                        )
                    )
                    ( Dict.empty, ( 1, rightId ) )
                    inloops
                    |> Tuple.first

            outloopNode =
                let
                    children =
                        List.map (\cid -> ( leftId cid, Just attachment )) outloop.roots
                in
                Dict.insert (baseId 0) (nodeOfShape (Sep children)) Dict.empty
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


hydrate : Net -> List Tree
hydrate net =
    net.roots
        |> List.filterMap (buildTree net)


buildTree : Net -> Id -> Maybe Tree
buildTree net id =
    Dict.get id net.nodes
        |> Maybe.map
            (\node ->
                let
                    resolvedChildren =
                        case node.shape of
                            Form _ ->
                                []

                            Sep children ->
                                children
                                    |> List.filterMap (\( cid, _ ) -> buildTree net cid)
                in
                TNode
                    { id = id
                    , node = node
                    , children = resolvedChildren
                    }
            )



-- Handling identifiers


idMapShape : (Id -> Id) -> Shape -> Shape
idMapShape f shape =
    case shape of
        Form _ ->
            shape

        Sep children ->
            Sep (List.map (Tuple.mapFirst f) children)


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

        Inside id ->
            Inside (f id)


idMapNode : (Id -> Id) -> Node -> Node
idMapNode f node =
    { node
        | shape = idMapShape f node.shape
        , justif = idMapJustif f node.justif
        , parent = idMapContext f node.parent
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
freshId { nodes } =
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
    ( [], maxId nodes + 1 )



{- `freshify s t` returns a net `t'` equal to `t` where all IDs have been changed so that they don't
   overlap with those in `s`.
-}


freshify : Net -> Net -> Net
freshify s t =
    idMapNet (Tuple.mapSecond (\i -> i + Tuple.second (freshId s))) t



-- Surgery
{- `prune net id` removes the substructure with root `id` in `net`, as well as associated
   justifications/interactions.
-}


prune : Net -> Id -> Net
prune net id =
    { nodes =
        Debug.todo ""
    , roots =
        Debug.todo ""
    }



{- `insert net loc tok` inserts token `tok` at location `loc` in `net`, by properly "dehydrating" it
   into a node with fresh IDs for every subnodes, as well as the correct attachment structure.
-}


insert : Net -> Location -> IToken -> Net
insert net { ctx, idx } tok =
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
                polarityOfContext net ctx
            }
    in
    { nodes =
        Debug.todo ""
    , roots =
        case ctx of
            TopLevel ->
                Utils.List.insert idx id net.roots

            _ ->
                net.roots
    }



-- Querying edit status


isInserted : Node -> Bool
isInserted node =
    node.polarity == Neg && node.justif.self


isDeleted : Node -> Bool
isDeleted node =
    node.polarity == Pos && node.justif.self


isDirectlyJustified : Node -> Bool
isDirectlyJustified node =
    case node.justif.from of
        Just (Direct _) ->
            True

        _ ->
            False


isIterated : Node -> Bool
isIterated node =
    node.polarity == Pos && isDirectlyJustified node


isDeiterated : Node -> Bool
isDeiterated node =
    node.polarity == Neg && isDirectlyJustified node


isExpansion : Polarity -> Interaction -> Bool
isExpansion pol int =
    (pol == Pos && int.opened) || (pol == Neg && int.closed)


isCollapse : Polarity -> Interaction -> Bool
isCollapse pol int =
    (pol == Pos && int.closed) || (pol == Neg && int.opened)


isCreated : Node -> Bool
isCreated node =
    isInserted node || isIterated node


isDestroyed : Node -> Bool
isDestroyed node =
    isDeleted node || isDeiterated node


isExpanded : Net -> Id -> Bool
isExpanded net id =
    getInteractions net id
        |> Dict.toList
        |> Utils.List.exists (\( _, int ) -> isExpansion (getPolarity net id) int)


isCollapsed : Net -> Id -> Bool
isCollapsed net id =
    getInteractions net id
        |> Dict.toList
        |> Utils.List.exists (\( _, int ) -> isCollapse (getPolarity net id) int)



-- Detours


isDetour : Net -> Id -> Bool
isDetour net id =
    let
        node =
            getNode net id
    in
    (isCreated node && isDestroyed node) || (isExpanded net id && isCollapsed net id)



{- A net is normal when it is detour-free -}


isNormal : Net -> Bool
isNormal net =
    net.nodes
        |> Dict.keys
        |> Utils.List.forall (not << isDetour net)



-- Boundaries


premiss : Net -> Struct
premiss net =
    Debug.todo ""
