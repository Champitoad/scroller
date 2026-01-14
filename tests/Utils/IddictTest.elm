module Utils.IddictTest exposing (..)

import Expect
import Iddict exposing (Iddict)
import Test exposing (..)
import Utils.Iddict exposing (dunion)


suite : Test
suite =
    describe "Utils.Iddict.dunion"
        [ test "disjoint sets" <|
            \_ ->
                let
                    -- d1 = {0: "A"}, d2 = {0: "B"}
                    -- Wait, if they are both created from empty, they both have key 0.
                    -- Let's make d2 have a different key.
                    ( _, d1_ ) =
                        Iddict.empty |> Iddict.insert "A"

                    ( key2_, _ ) =
                        d1_ |> Iddict.insert "B"

                    d2_only =
                        Iddict.empty |> Iddict.update key2_ (\_ -> Just "B")

                    result =
                        dunion d1_ d2_only
                in
                result
                    |> Iddict.toList
                    |> Expect.equalLists [ ( 0, "A" ), ( 1, "B" ) ]
        , test "overlapping sets" <|
            \_ ->
                let
                    ( _, d1 ) =
                        Iddict.empty |> Iddict.insert "A"

                    ( _, d2 ) =
                        Iddict.empty |> Iddict.insert "B"

                    -- d1 = {0: "A"}, d2 = {0: "B"}
                    result =
                        dunion d1 d2
                in
                result
                    |> Iddict.toList
                    -- k=0 from d1 stays 0, k=0 from d2 gets new key 1
                    |> Expect.equalLists [ ( 0, "A" ), ( 1, "B" ) ]
        , test "multiple overlaps" <|
            \_ ->
                let
                    d1 =
                        Iddict.empty
                            |> Iddict.update 0 (\_ -> Just "A")
                            |> Iddict.update 1 (\_ -> Just "B")

                    d2 =
                        Iddict.empty
                            |> Iddict.update 0 (\_ -> Just "C")
                            |> Iddict.update 1 (\_ -> Just "D")

                    result =
                        dunion d1 d2
                in
                result
                    |> Iddict.toList
                    -- 0, 1 from d1 stay.
                    -- 0, 1 from d2 get new keys. max(1,1)+1 = 2.
                    -- 0 -> 2, 1 -> 3
                    |> Expect.equalLists [ ( 0, "A" ), ( 1, "B" ), ( 2, "C" ), ( 3, "D" ) ]
        , test "empty sets" <|
            \_ ->
                let
                    ( _, d1 ) =
                        Iddict.empty |> Iddict.insert "A"

                    result =
                        dunion d1 Iddict.empty
                in
                result
                    |> Iddict.toList
                    |> Expect.equalLists [ ( 0, "A" ) ]
        ]
