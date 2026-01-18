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
    = ITok OToken
    | ISep (List IToken)



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


type Shape
    = Form Formula
    | Sep (List Id)


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


type Context
    = TopLevel
    | Inside Id


type alias Location =
    { ctx : Context, idx : Int }


type alias Node =
    { shape : Shape
    , name : String
    , justif : Justification
    , parent : Context
    , polarity : Polarity
    }


type alias Net =
    { nodes : Dict Id Node
    , roots : List Id
    , interactions : Dict ( Id, Id ) Interaction -- Edges oriented from leaves to roots
    }



-- Basic operations


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


getNode : Net -> Id -> Node
getNode net id =
    Dict.get id net.nodes
        |> Maybe.withDefault
            { shape = Form (Atom (Image { src = "node-not-found.png", description = "Node not found!" }))
            , name = ""
            , parent = TopLevel
            , polarity = Pos
            , justif = assumption
            }


getInteractions : Net -> Id -> Dict Id Interaction
getInteractions net id =
    Debug.todo ""


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

                            Sep childIds ->
                                childIds
                                    |> List.filterMap (buildTree net)
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
            Sep (List.map f children)


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


pairIdMapDict : ((Id -> Id) -> a -> a) -> (Id -> Id) -> Dict ( Id, Id ) a -> Dict ( Id, Id ) a
pairIdMapDict vmap f dict =
    dict
        |> Dict.toList
        |> List.map (\( ( id1, id2 ), v ) -> ( ( f id1, f id2 ), vmap f v ))
        |> Dict.fromList


idMapNet : (Id -> Id) -> Net -> Net
idMapNet f net =
    { nodes = idMapDict idMapNode f net.nodes
    , roots = List.map f net.roots
    , interactions = pairIdMapDict (\_ -> identity) f net.interactions
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
    , interactions = Dict.empty
    }



{- `fo form` returns the singleton net with formula `form` -}


fo : Formula -> Net
fo form =
    { nodes = Dict.fromList [ ( baseId 0, nodeOfShape (Form form) ) ]
    , roots = [ baseId 0 ]
    , interactions = Dict.empty
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
    , interactions =
        Dict.union
            (pairIdMapDict (\_ -> identity) leftId s.interactions)
            (pairIdMapDict (\_ -> identity) rightId t.interactions)
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

            outloopNode =
                Dict.insert (baseId 0) (nodeOfShape (Sep (List.map leftId outloop.roots))) Dict.empty

            inloopsNodes =
                List.foldl
                    (\inloop ( acc, ( idx, inj ) ) ->
                        ( Dict.insert (baseId idx) (nodeOfShape (Sep (List.map inj inloop.roots))) acc
                        , ( idx + 1, inj >> rightId )
                        )
                    )
                    ( Dict.empty, ( 1, rightId ) )
                    inloops
                    |> Tuple.first
        in
        List.foldl Dict.union Dict.empty [ outloopContent, inloopsContents, outloopNode, inloopsNodes ]
    , roots =
        [ baseId 0 ]
    , interactions =
        let
            outloopInteractions =
                pairIdMapDict (\_ -> identity) leftId outloop.interactions

            inloopsInteractions =
                List.foldl
                    (\inloop ( acc, inj ) ->
                        ( Dict.union acc (pairIdMapDict (\_ -> identity) inj inloop.interactions), inj >> rightId )
                    )
                    ( Dict.empty, rightId )
                    inloops
                    |> Tuple.first

            attachments =
                List.range 0 (List.length inloops)
                    |> List.map (\i -> ( ( baseId 0, baseId (i + 1) ), attachment ))
                    |> Dict.fromList
        in
        Dict.union outloopInteractions (Dict.union inloopsInteractions attachments)
    }



-- Boundaries and detours


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
