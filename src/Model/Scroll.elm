module Model.Scroll exposing (..)

import Dict exposing (Dict)
import Model.Formula exposing (..)



{- Outer token -}


type OToken
    = OForm Formula
    | OSep (List IToken)



{- Inner token -}


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
    = Formula Formula
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
    | InSep Id


type alias Location =
    { ctx : Context, idx : Int }


type alias Node =
    { shape : Shape
    , name : String
    , justif : Justification
    , parent : Context
    }


type alias Net =
    { nodes : Dict Id Node
    , roots : List Id
    , interactions : Dict ( Id, Id ) Interaction -- Edges oriented from leaves to roots<@
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


getParent : Net -> Id -> Context
getParent net id =
    Dict.get id net.nodes
        |> Maybe.map .parent
        |> Maybe.withDefault TopLevel


getPolarity : Net -> Id -> Polarity
getPolarity net id =
    case getParent net id of
        TopLevel ->
            Pos

        InSep par ->
            invert (getPolarity net par)



-- Hydration pattern


type Tree
    = TNode
        { id : Id
        , node : Node
        , polarity : Polarity
        , children : List Tree
        }


hydrate : Net -> List Tree
hydrate net =
    net.roots
        |> List.filterMap (buildTree Pos net)


buildTree : Polarity -> Net -> Id -> Maybe Tree
buildTree polarity net id =
    Dict.get id net.nodes
        |> Maybe.map
            (\node ->
                let
                    resolvedChildren =
                        case node.shape of
                            Formula _ ->
                                []

                            Sep childIds ->
                                childIds
                                    |> List.filterMap (buildTree (invert polarity) net)
                in
                TNode
                    { id = id
                    , node = node
                    , polarity = polarity
                    , children = resolvedChildren
                    }
            )



-- Handling identifiers


idMapShape : (Id -> Id) -> Shape -> Shape
idMapShape f shape =
    case shape of
        Formula _ ->
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

        InSep id ->
            InSep (f id)


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
    { shape = shape, name = "", justif = assumption, parent = TopLevel }



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
    { nodes = Dict.fromList [ ( baseId 0, nodeOfShape (Formula form) ) ]
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
