module EncloseTest exposing (..)

import Dict
import Expect
import List.Extra
import Model.Formula as Formula
import Model.Scroll as Scroll exposing (..)
import Test exposing (..)
import Utils.List


suite : Test
suite =
    describe "Scroll Enclosure"
        [ test "Enclose contiguous nodes at top level" <|
            \_ ->
                let
                    -- Structure: [ A, B, C ]
                    struct =
                        [ Scroll.a "A"
                        , Scroll.a "B"
                        , Scroll.a "C"
                        ]

                    net =
                        Scroll.netOfStruct struct

                    -- IDs are likely 1, 2, 3 (hydration starts at 0, root is usually last or something? No, hydration uses State)
                    -- Roots: let's inspect
                    rootIds =
                        net.roots

                    -- Assume we want to enclose B (index 1)
                    nodesToEnclose =
                        case rootIds of
                            [ _, b, _ ] ->
                                [ b ]

                            _ ->
                                []

                    ( newId, newNet ) =
                        Scroll.enclose nodesToEnclose Nothing net
                in
                if List.isEmpty nodesToEnclose then
                    Expect.fail "Setup failed, unexpected root structure"

                else
                    let
                        parentRoots =
                            newNet.roots

                        -- Should be [A, NewNode, C]
                        -- NewNode should be Sep [B] Nothing
                        -- Check parent roots length is 3
                        checkLength =
                            Expect.equal 3 (List.length parentRoots)

                        -- Check middle node is the new ID
                        checkMiddle =
                            case parentRoots of
                                [ _, mid, _ ] ->
                                    Expect.equal mid newId

                                _ ->
                                    Expect.fail "Unexpected root structure after enclose"

                        -- Check new node content
                        checkNewNode =
                            case Scroll.getShape newId newNet of
                                Sep children interaction ->
                                    Expect.all
                                        [ \_ -> Expect.equal children nodesToEnclose
                                        , \_ -> Expect.equal interaction Nothing
                                        ]
                                        ()

                                _ ->
                                    Expect.fail "New node is not a Sep"

                        -- Check context of B
                        checkContext =
                            List.all (\id -> Scroll.getContext id newNet == Inside newId) nodesToEnclose
                                |> Expect.equal True
                    in
                    Expect.all
                        [ \_ -> checkLength
                        , \_ -> checkMiddle
                        , \_ -> checkNewNode
                        , \_ -> checkContext
                        ]
                        ()
        , test "Enclose contiguous sequence [A, B]" <|
            \_ ->
                let
                    struct =
                        [ Scroll.a "A", Scroll.a "B", Scroll.a "C" ]

                    net =
                        Scroll.netOfStruct struct

                    rootIds =
                        net.roots

                    nodesToEnclose =
                        List.take 2 rootIds

                    ( newId, newNet ) =
                        Scroll.enclose nodesToEnclose Nothing net
                in
                let
                    parentRoots =
                        newNet.roots

                    -- Should be [NewNode, C]
                in
                Expect.all
                    [ \_ -> Expect.equal 2 (List.length parentRoots)
                    , \_ -> Expect.equal (List.head parentRoots) (Just newId)
                    ]
                    ()
        , test "Double Enclose (Open logic)" <|
            \_ ->
                let
                    struct =
                        [ Scroll.a "A" ]

                    net =
                        Scroll.netOfStruct struct

                    rootIds =
                        net.roots

                    selection =
                        rootIds

                    ( inloopId, net1 ) =
                        Scroll.enclose selection (Just Scroll.attachment) net

                    ( outloopId, net2 ) =
                        Scroll.enclose [ inloopId ] Nothing net1

                    roots =
                        net2.roots
                in
                case roots of
                    [ root ] ->
                        if root == outloopId then
                            case Scroll.getShape outloopId net2 of
                                Sep [ child ] Nothing ->
                                    if child == inloopId then
                                        case Scroll.getShape inloopId net2 of
                                            Sep [ grandchild ] (Just _) ->
                                                if Just grandchild == List.head selection then
                                                    -- Check polarity of grandchild (A)
                                                    -- Original A was at root, so Pos.
                                                    -- Current A is inside Inloop inside Outloop.
                                                    -- Outloop is at root, so Pos.
                                                    -- Inloop is inside Outloop, so Neg.
                                                    -- A is inside Inloop, so Pos.
                                                    -- So A polarity should be Pos.
                                                    let
                                                        polA =
                                                            Scroll.getPolarity grandchild net2
                                                    in
                                                    if polA == Scroll.Pos then
                                                        -- Check polarity of Inloop
                                                        let
                                                            polInloop =
                                                                Scroll.getPolarity inloopId net2
                                                        in
                                                        if polInloop == Scroll.Neg then
                                                            Expect.pass

                                                        else
                                                            Expect.fail "Inloop polarity should be Neg"

                                                    else
                                                        Expect.fail "Grandchild polarity should be Pos"

                                                else
                                                    Expect.fail "Grandchild is not A"

                                            _ ->
                                                Expect.fail "Inloop is not a Sep with interaction"

                                    else
                                        Expect.fail "Outloop child is not inloopId"

                                _ ->
                                    Expect.fail "Outloop is not Sep with Nothing"

                        else
                            Expect.fail "Root is not outloopId"

                    _ ->
                        Expect.fail "Expected single root"
        ]
