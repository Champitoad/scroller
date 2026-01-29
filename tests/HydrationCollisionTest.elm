module HydrationCollisionTest exposing (..)

import Dict
import Expect
import Model.Formula as Formula
import Model.Scroll as Scroll exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Scroll Hydration Collision"
        [ test "No ID collisions with nested structures" <|
            \_ ->
                let
                    -- Structure: [ OSep [ ISep [] ], OForm "a" ]
                    -- Expected:
                    -- Node A (Sep)
                    --   Node A.Child (Sep)
                    -- Node B (Form)
                    -- Total 3 unique nodes.
                    -- Current buggy implementation might assign same ID to A.Child and B.
                    struct =
                        [ OSep [ ISep [] ]
                        , OForm (Formula.atom "a")
                        ]

                    net =
                        Scroll.netOfStruct struct
                in
                Dict.size net.nodes
                    |> Expect.equal 3
        ]
