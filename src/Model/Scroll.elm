module Model.Scroll exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Iddict exposing (Iddict)
import Model.Formula exposing (..)



-- Fundamental data structures


type alias Id =
    Int


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
    { nodes : Iddict Node
    , topLevel : Array Id
    , interaction : Dict ( Id, Id ) Interaction
    }



-- Constructors


assumption : Justification
assumption =
    { self = False, from = Nothing, copy = Nothing }


nodeOfShape : Shape -> Node
nodeOfShape shape =
    { shape = shape
    , name = ""
    , justif = assumption
    }



-- Basic operations


parent : Net -> Id -> Maybe Id
parent net id =
    Iddict.foldl
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
