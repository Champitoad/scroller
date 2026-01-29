module View.Manual exposing (..)

import Element exposing (..)
import Element.Input as Input
import FeatherIcons as Icons
import Model.App exposing (..)
import Model.Scroll exposing (..)
import Model.Session exposing (..)
import Update.App exposing (..)
import View.Session exposing (..)
import View.Style as Style exposing (..)


mkIcon : Float -> Icons.Icon -> Element msg
mkIcon size icon =
    icon
        |> Icons.withSize size
        |> Icons.toHtml []
        |> html
        |> el []


headingIconSize : Float
headingIconSize =
    40


flowerIcon : Element msg
flowerIcon =
    el [ fontSize (round headingIconSize + 10) ] (text "❀")


proofIcon : Element msg
proofIcon =
    mkIcon headingIconSize Icons.checkSquare


editIcon : Element msg
editIcon =
    mkIcon headingIconSize Icons.edit2


navigateIcon : Element msg
navigateIcon =
    mkIcon headingIconSize Icons.navigation


resetIcon : Element msg
resetIcon =
    mkIcon 20 Icons.rotateCw


resetButton : SandboxID -> Element Msg
resetButton id =
    Input.button
        []
        { onPress = Just (ResetSandbox id)
        , label = resetIcon
        }


viewSandbox : DnD -> Sandbox -> String -> Element Msg
viewSandbox dnd { currentSession } id =
    row
        [ width fill
        , spacing 20
        , padding 5
        ]
        [ viewSession dnd currentSession
        , resetButton id
        ]


backButtonStyle : List (Attribute msg)
backButtonStyle =
    [ styleAttr "border" "2px solid black"
    , styleAttr "border-radius" "4px"
    , padding 10
    ]


navbar : Element msg
navbar =
    row
        [ width fill
        , padding 20
        ]
        [ link
            (alignLeft :: backButtonStyle)
            { url = "/", label = text "back to app" }
        ]


body : Model -> Element Msg
body { manualExamples, dragDrop } =
    let
        sandbox id =
            viewSandbox dragDrop (getSandbox id manualExamples) id

        padder =
            el [ width shrink, height fill ] none

        ( tcol, par ) =
            let
                fontSize =
                    20
            in
            ( textColumn [ spacing 10, Style.fontSize fontSize ]
            , paragraph [ Style.fontSize fontSize ]
            )

        ( t, b, i ) =
            ( text, bold, italic )

        h1 icon txt =
            row [ spacing 20, styleAttr "padding" "20px 0px 10px 0px" ]
                [ icon
                , el [ fontSize 40 ] (text txt)
                ]

        h2 txt =
            row [ spacing 15, paddingEach { top = 10, bottom = 0, left = 0, right = 0 } ]
                [ el [ fontSize 35 ] (text "•")
                , el [ fontSize 25 ] (bold txt)
                ]
    in
    row
        (scrollbarY
            :: fillXY
        )
        [ padder
        , column
            [ width (fill |> maximum 600)
            , height fill
            , padding 20
            , spacing 30
            , centerX
            ]
            [ h1 flowerIcon "Flowers"
            , par [ t "Look at this box:" ]
            , sandbox "Flower"
            , tcol
                [ par [ t "It is called a ", b "flower", t "." ]
                , par [ t "a, b, c and d are ", b "atoms", t "." ]
                , par [ t "a and b are in the ", b "outloop", t " (upper part of the box)." ]
                , par [ t "c and d are each in a ", b "inloop", t "." ]
                , par [ t "Inloops form the ", b "corolla", t " (lower part of the box)." ]
                ]
            , tcol
                [ par [ t "Flowers represent ", b "logical statements", t ":" ]
                , textColumn [ paddingXY 30 5, spacing 5 ]
                    [ par [ i "If a ", b "and", i " b are true," ]
                    , par [ b "then", i " either c ", b "or", i " d is true." ]
                    ]
                , par [ t "Juxtaposition is interpreted as ", b "conjunction", t "." ]
                , par [ t "A outloop ", b "implies", t " the ", b "disjunction", t " of its inloops." ]
                ]
            , par [ t "Flowers can be ", b "proved", t ", ", b "edited", t " and ", b "navigated", t "." ]
            , h1 proofIcon "Proof Mode"
            , h2 "QED"
            , par [ t "Click on an ", el greenActionable.active (text "empty inloop"), t " to erase its flower." ]
            , sandbox "QED"
            , par [ t "Note that emptiness is interpreted as ", b "truth", t "." ]
            , h2 "Justify"
            , par [ t "Click on an ", el greenActionable.active (text "atom"), t " to erase it." ]
            , sandbox "Justify"
            , h2 "Unlock"
            , par [ t "Click on an ", el orangeActionable.active (text "empty outloop"), t " to unlock its inloop." ]
            , sandbox "Unlock"
            , par [ t "Note that flowers can be ", b "nested", t " inside each other." ]
            , h2 "Import"
            , par
                [ t "Drag a "
                , el (draggable useColor).active (text "flower")
                , t " to copy it, and drop it in an "
                , el (droppable useColor).active (text "area")
                , t " to paste it."
                ]
            , sandbox "Import"
            , h2 "Case"
            , par [ t "Click on an ", el orangeActionable.active (text "empty outloop"), t " to turn its attached inloops into outloops." ]
            , sandbox "Case"
            , h2 "Decompose"
            , par [ t "Click on a ", el pinkActionable.active (text "symbolic formula"), t " to turn it into a flower." ]
            , sandbox "Decompose"
            , h1 editIcon "Edit Mode"
            , par [ t "Coming soon!" ]
            , h1 navigateIcon "Navigation Mode"
            , par [ t "Coming soon!" ]
            ]
        , padder
        ]


page : Model -> Element Msg
page model =
    column
        [ width fill
        , height fill
        ]
        [ body model
        , navbar
        ]
