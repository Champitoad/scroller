module Model.Scroll exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Model.Formula exposing (..)



-- Identifiers


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
    | Sep (Array Id)


type alias Justification =
    { self : Bool
    , from : Maybe Id
    , copy : Maybe Id
    }


type alias Interaction =
    { attached : Bool
    , opened : Bool
    , closed : Bool
    }


type alias Node =
    { shape : Shape
    , name : String
    , justif : Justification
    }


type alias Net =
    { nodes : Dict Id Node
    , roots : Array Id
    , interactions : Dict ( Id, Id ) Interaction
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



-- Combine


idMapShape : (Id -> Id) -> Shape -> Shape
idMapShape f shape =
    case shape of
        Formula _ ->
            shape

        Sep children ->
            Sep (Array.map f children)


idMapJustif : (Id -> Id) -> Justification -> Justification
idMapJustif f justif =
    { justif
        | from = Maybe.map f justif.from
        , copy = Maybe.map f justif.copy
    }


idMapNode : (Id -> Id) -> Node -> Node
idMapNode f node =
    { node
        | shape = idMapShape f node.shape
        , justif = idMapJustif f node.justif
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



-- Constructors


assumption : Justification
assumption =
    { self = False, from = Nothing, copy = Nothing }


attachment : Interaction
attachment =
    { attached = True, opened = False, closed = False }


nodeOfShape : Shape -> Node
nodeOfShape shape =
    { shape = shape, name = "", justif = assumption }



{- The empty scroll net -}


empty : Net
empty =
    { nodes = Dict.empty
    , roots = Array.empty
    , interactions = Dict.empty
    }



{- `f form` returns the singleton net with formula `form` -}


fo : Formula -> Net
fo form =
    { nodes = Dict.fromList [ ( baseId 0, nodeOfShape (Formula form) ) ]
    , roots = Array.fromList [ baseId 0 ]
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
        Array.append
            (Array.map leftId s.roots)
            (Array.map rightId t.roots)
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
                Dict.insert (baseId 0) (nodeOfShape (Sep (Array.map leftId outloop.roots))) Dict.empty

            inloopsNodes =
                List.foldl
                    (\inloop ( acc, ( idx, inj ) ) ->
                        ( Dict.insert (baseId idx) (nodeOfShape (Sep (Array.map inj inloop.roots))) acc
                        , ( idx + 1, inj >> rightId )
                        )
                    )
                    ( Dict.empty, ( 1, rightId ) )
                    inloops
                    |> Tuple.first
        in
        List.foldl Dict.union Dict.empty [ outloopContent, inloopsContents, outloopNode, inloopsNodes ]
    , roots =
        Array.push (baseId 0) Array.empty
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
                Array.initialize
                    (List.length inloops)
                    (\i -> ( ( baseId 0, baseId (i + 1) ), attachment ))
                    |> Array.toList
                    |> Dict.fromList
        in
        Dict.union outloopInteractions (Dict.union inloopsInteractions attachments)
    }



-- Basic operations


parent : Net -> Id -> Maybe Id
parent net id =
    Dict.foldl
        (\pId node acc ->
            case node.shape of
                Sep children ->
                    if Array.toList children |> List.member id then
                        Just pId

                    else
                        acc

                _ ->
                    acc
        )
        Nothing
        net.nodes
