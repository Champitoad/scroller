module View.Toolbar exposing (..)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Input as Input
import FeatherIcons as Icons
import Html.Attributes
import Model.App exposing (..)
import Model.Session exposing (..)
import Queue
import Update.App exposing (..)
import Utils.Color
import Utils.Events exposing (..)
import Utils.Maybe exposing (..)
import View.Style exposing (..)
import View.Widgets exposing (..)


type ModeSelectorPosition
    = Start
    | Middle
    | End


modeSelectorColor : Color
modeSelectorColor =
    rgb 0.5 0.5 0.5


modeSelectorBorderRound : ModeSelectorPosition -> String
modeSelectorBorderRound position =
    let
        r =
            String.fromInt buttonBorderRadius ++ "px"
    in
    case position of
        Start ->
            r ++ " 0px 0px " ++ r

        Middle ->
            "0px 0px 0px 0px"

        End ->
            "0px " ++ r ++ " " ++ r ++ " 0px"


viewAutoButton : ActionMode -> Element Msg
viewAutoButton mode =
    let
        enabled =
            case mode of
                ProofMode _ ->
                    True

                _ ->
                    False
    in
    defaultButton
        { action = Msg Auto
        , title = "Auto"
        , icon = Icons.zap
        , enabled = enabled
        }


viewHelpButton : Element Msg
viewHelpButton =
    defaultButton
        { action = Link "/manual"
        , title = "Help"
        , icon = Icons.helpCircle
        , enabled = True
        }


viewNewAtomNameTextEdit : String -> Element Msg
viewNewAtomNameTextEdit newAtomName =
    Input.text
        [ styleAttr "border-radius" (String.fromInt sepBorderRound ++ "px")
        , onClick DoNothing
        ]
        { onChange = UpdateNewAtomName
        , text = newAtomName
        , placeholder = Just (Input.placeholder [] (text "New atom name"))
        , label = Input.labelHidden "New atom name"
        }


viewActionModeSelector : ActionMode -> Element Msg
viewActionModeSelector currentMode =
    let
        item mode position =
            let
                isSelected =
                    case ( mode, currentMode ) of
                        ( ProofMode _, ProofMode _ ) ->
                            True

                        ( EditMode _, EditMode _ ) ->
                            True

                        _ ->
                            mode == currentMode

                ( bgColor, fgColor ) =
                    if isSelected then
                        ( modeSelectorColor, rgb 1 1 1 )

                    else
                        ( rgb 1 1 1, rgb 0 0 0 )

                ( iconEl, titleText ) =
                    let
                        ( title, icon ) =
                            case mode of
                                ProofMode _ ->
                                    ( "Prove", Icons.checkSquare )

                                EditMode _ ->
                                    ( "Edit", Icons.edit2 )

                                NavigationMode ->
                                    ( "Navigate", Icons.navigation )

                        elem =
                            el
                                [ centerX, centerY ]
                                (icon
                                    |> Icons.withSize 30
                                    |> Icons.toHtml
                                        [ fgColor
                                            |> Utils.Color.fromElement
                                            |> Utils.Color.htmlAttr
                                        ]
                                    |> html
                                )
                    in
                    ( elem, title )

                borderRound =
                    modeSelectorBorderRound position

                changeAction =
                    [ Events.onClick (ChangeActionMode mode)
                    , pointer
                    ]
            in
            el
                ([ width (60 |> px)
                 , height (defaultButtonSize |> px)
                 , Background.color bgColor
                 , styleAttr "border-radius" borderRound
                 , htmlAttribute <| Html.Attributes.title titleText
                 ]
                    ++ changeAction
                )
                iconEl

        borderColor =
            rgb 0.6 0.6 0.6
    in
    row
        [ width shrink
        , height shrink
        , spacing 1
        , styleAttr "border-width" "0px"
        , styleAttr "border-radius" (String.fromInt buttonBorderRadius ++ "px")
        , styleAttr "border-color" "rgb(153, 153, 153)"
        , Background.color borderColor
        ]
        [ item (ProofMode Justifying) Start
        , item
            (EditMode
                { interaction = Operating
                , newAtomName = ""
                , insertions = Dict.empty
                }
            )
            Middle
        , item NavigationMode End
        ]


viewUndoRedo : History -> Element Msg
viewUndoRedo (History history) =
    let
        ( undoEnabled, redoEnabled ) =
            Tuple.mapBoth isSomething isSomething ( history.prev, history.next )
    in
    row []
        [ defaultButton
            { action = Msg Undo
            , title = "Undo"
            , icon = Icons.arrowLeft
            , enabled = undoEnabled
            }
        , defaultButton
            { action = Msg Redo
            , title = "Redo"
            , icon = Icons.arrowRight
            , enabled = redoEnabled
            }
        ]


viewExecModeSelector : ExecMode -> Element Msg
viewExecModeSelector currentMode =
    let
        item mode position =
            let
                isSelected =
                    mode == currentMode

                ( bgColor, fgColor ) =
                    if isSelected then
                        ( modeSelectorColor, rgb 1 1 1 )

                    else
                        ( rgb 1 1 1, rgb 0 0 0 )

                ( iconEl, titleText ) =
                    let
                        ( title, icon ) =
                            case mode of
                                Forward ->
                                    ( "Forward", Icons.chevronsRight )

                                Backward ->
                                    ( "Backward", Icons.chevronsLeft )

                        elem =
                            el
                                [ centerX, centerY ]
                                (icon
                                    |> Icons.withSize 30
                                    |> Icons.toHtml
                                        [ fgColor
                                            |> Utils.Color.fromElement
                                            |> Utils.Color.htmlAttr
                                        ]
                                    |> html
                                )
                    in
                    ( elem, title )

                borderRound =
                    modeSelectorBorderRound position

                changeAction =
                    [ Events.onClick (ChangeExecMode mode)
                    , pointer
                    ]
            in
            el
                ([ width (60 |> px)
                 , height (defaultButtonSize |> px)
                 , Background.color bgColor
                 , styleAttr "border-radius" borderRound
                 , htmlAttribute <| Html.Attributes.title titleText
                 ]
                    ++ changeAction
                )
                iconEl

        borderColor =
            rgb 0.6 0.6 0.6
    in
    row
        [ width shrink
        , height shrink
        , spacing 1
        , styleAttr "border-width" "0px"
        , styleAttr "border-radius" (String.fromInt buttonBorderRadius ++ "px")
        , styleAttr "border-color" "rgb(153, 153, 153)"
        , Background.color borderColor
        ]
        [ item Backward Start
        , item Forward End
        ]


viewRecordToggle : Bool -> Element Msg
viewRecordToggle recording =
    toggle
        { color = modeSelectorColor
        , iconOn = Icons.camera
        , iconOff = Icons.cameraOff
        , title = "Record"
        , onChange = ToggleRecording
        }
        recording


viewExecButtons : Session -> Element Msg
viewExecButtons { actionsQueue } =
    let
        stepEnabled =
            not (Queue.isEmpty actionsQueue)

        stepButton =
            defaultButton
                { action = Msg Step
                , title = "Step"
                , icon = Icons.play
                , enabled = stepEnabled
                }

        runButton =
            defaultButton
                { action = Msg ExecAll
                , title = "Run"
                , icon = Icons.skipForward
                , enabled = True
                }
    in
    row [] [ stepButton, runButton ]


toolbarPadding : Int
toolbarPadding =
    15


toolbarHeight : Int
toolbarHeight =
    defaultButtonSize + 2 * toolbarPadding


viewToolbar : Model -> Element Msg
viewToolbar model =
    let
        -- autoButton = viewAutoButton model.playground.actionMode
        helpButton =
            viewHelpButton

        actionModeSelector =
            viewActionModeSelector model.playground.actionMode

        undoRedo =
            viewUndoRedo model.history

        actionToolZone =
            row
                [ paddingXY 15 0
                , spacing 10
                , centerX
                ]
                (case model.playground.actionMode of
                    EditMode { newAtomName } ->
                        [ el
                            [ width (fill |> maximum 200)
                            , centerX
                            ]
                            (viewNewAtomNameTextEdit newAtomName)
                        ]

                    _ ->
                        []
                )

        execZone =
            row
                [ paddingXY 15 0
                , spacing 20
                , centerX
                ]
                [ viewRecordToggle model.playground.recording
                , viewExecModeSelector model.playground.execMode
                , viewExecButtons model.playground
                ]
    in
    row
        [ width fill
        , height (toolbarHeight |> px)
        , padding toolbarPadding
        , spacing 100
        , styleAttr "border-width" "1px 0px 0px 0px"
        , styleAttr "border-style" "solid"
        , styleAttr "border-color" "rgb(153, 153, 153)"

        -- , Border.shadow
        --     { offset = (0, -3)
        --     , size = 0.1
        --     , blur = 5
        --     , color = rgb 0.5 0.5 0.5 }
        , Background.gradient
            { angle = 0
            , steps = [ rgb 0.8 0.8 0.8, rgb 0.9 0.9 0.9 ]
            }
        ]
        [ el [ alignLeft ] helpButton
        , el [ width fill ] actionToolZone
        , el [ centerX ] actionModeSelector
        , el [ width fill ] execZone
        , el [ alignRight ] undoRedo
        ]
