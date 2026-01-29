module HydrationFoldTest exposing (..)

import Dict
import Expect
import Model.Formula as Formula
import Model.Scroll as Scroll exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Scroll Hydration Fold"
        [ test "Result matches original structure (ordered)" <|
            \_ ->
                let
                    -- Structure: [ Form A, Form B ]
                    -- Expected: Tree A, Tree B in that order.
                    struct =
                        [ OForm (Formula.atom "a")
                        , OForm (Formula.atom "b")
                        ]

                    trees =
                        Scroll.hydrateStruct struct
                in
                List.map (\tree -> (Scroll.getTreeNode tree).shape) trees
                    |> Expect.equal [ Formula (Formula.atom "a"), Formula (Formula.atom "b") ]
        ]
