module View.Widgets exposing (..)

import Color
import Css
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icons exposing (Icon)
import Html.Attributes
import Html.Styled exposing (fromUnstyled, toUnstyled)
import Html.Styled.Attributes as Attrs exposing (action, css)
import Model.Scroll exposing (Id)
import Update.App exposing (Msg(..))
import Utils.Color
import Utils.Events
import View.Style exposing (..)



-- Buttons


buttonBorderRadius : number
buttonBorderRadius =
    10


defaultButtonSize : number
defaultButtonSize =
    55


type alias ButtonStyle widthUnit heightUnit =
    { width : Css.LengthOrAuto widthUnit
    , height : Css.LengthOrAuto heightUnit
    , color : Color.Color
    , iconColorEnabled : Color.Color
    , iconColorDisabled : Color.Color
    }


type ButtonAction msg
    = Msg msg
    | Link String


type alias ButtonParams msg =
    { action : ButtonAction msg
    , title : String
    , content : Element msg
    , enabled : Bool
    }


type alias IconButtonParams msg =
    { action : ButtonAction msg
    , title : String
    , icon : Icon
    , enabled : Bool
    }


button : ButtonStyle widthUnit heightUnit -> ButtonParams msg -> Element msg
button style { action, title, content, enabled } =
    let
        iconStyledHtml =
            centered content |> layout [] |> fromUnstyled

        styleAttrs =
            Css.width style.width
                :: Css.height style.height
                :: Css.minWidth (Css.px defaultButtonSize)
                :: Css.minHeight (Css.px defaultButtonSize)
                :: Css.borderStyle Css.solid
                :: Css.borderRadius (Css.px buttonBorderRadius)
                :: Css.borderWidth (Css.px 1)
                :: Css.displayFlex
                :: Css.alignItems Css.center
                :: Css.justifyContent Css.center
                :: (if enabled then
                        [ Css.cursor Css.pointer
                        , Css.borderColor (style.color |> Utils.Color.changeLight 0.7 |> Utils.Color.toCss)
                        , Css.backgroundColor (style.color |> Utils.Color.toCss)
                        , Css.hover
                            [ Css.backgroundColor
                                (style.color |> Utils.Color.changeLight 1.15 |> Utils.Color.toCss)
                            ]
                        , Css.active
                            [ Css.backgroundColor
                                (style.color |> Utils.Color.changeLight 0.7 |> Utils.Color.toCss)
                            ]
                        ]

                    else
                        [ Css.borderColor Css.transparent
                        , Css.backgroundColor Css.transparent
                        ]
                   )

        tag =
            case action of
                Msg _ ->
                    Html.Styled.div

                Link _ ->
                    Html.Styled.a

        actionAttr =
            if enabled then
                case action of
                    Msg msg ->
                        [ Utils.Events.onClickStyled msg ]

                    Link url ->
                        [ Attrs.href url ]

            else
                []
    in
    tag
        (css styleAttrs :: Attrs.title title :: actionAttr)
        [ iconStyledHtml ]
        |> toUnstyled
        |> html


iconButton : ButtonStyle widthUnit heightUnit -> IconButtonParams msg -> Element msg
iconButton style { action, title, icon, enabled } =
    let
        color =
            if enabled then
                style.iconColorEnabled

            else
                style.iconColorDisabled
    in
    button
        style
        { action = action
        , title = title
        , content =
            icon
                |> Icons.withSize 30
                |> Icons.toHtml [ color |> Utils.Color.htmlAttr ]
                |> Element.html
        , enabled = enabled
        }


defaultButtonStyle : ButtonStyle Css.Px Css.Px
defaultButtonStyle =
    { width = Css.px defaultButtonSize
    , height = Css.px defaultButtonSize
    , color = Color.rgb 0.92 0.92 0.92
    , iconColorEnabled = Color.black
    , iconColorDisabled = Color.rgb 0.5 0.5 0.5
    }


defaultButton : IconButtonParams msg -> Element msg
defaultButton =
    iconButton defaultButtonStyle



-- Toggles


type alias ToggleParams msg =
    { color : Element.Color
    , iconOn : Icon
    , iconOff : Icon
    , title : String
    , onChange : Bool -> msg
    }


toggle : ToggleParams msg -> Bool -> Element msg
toggle { color, iconOn, iconOff, title, onChange } checked =
    let
        iconEl isChecked =
            let
                ( bgColor, fgColor ) =
                    if isChecked then
                        ( color, rgb 1 1 1 )

                    else
                        ( rgb 1 1 1, rgb 0 0 0 )
            in
            el
                [ width (60 |> px)
                , height (defaultButtonSize |> px)
                , Background.color bgColor
                , styleAttr "border-radius" (String.fromInt buttonBorderRadius ++ "px")
                , htmlAttribute <| Html.Attributes.title title
                ]
                (el
                    [ centerX, centerY ]
                    ((if isChecked then
                        iconOn

                      else
                        iconOff
                     )
                        |> Icons.withSize 30
                        |> Icons.toHtml
                            [ fgColor
                                |> Utils.Color.fromElement
                                |> Utils.Color.htmlAttr
                            ]
                        |> html
                    )
                )
    in
    Input.checkbox []
        { onChange = onChange
        , icon = iconEl
        , checked = checked
        , label = Input.labelHidden "Recording"
        }



-- Indicators


indicatorHeight : Int
indicatorHeight =
    25


indicatorIcon : Icon -> Element msg
indicatorIcon icon =
    icon
        |> Icons.withSize 20
        |> Icons.toHtml [ Color.white |> Utils.Color.htmlAttr ]
        |> html


indicator : Color -> Element msg -> Element msg
indicator color content =
    el
        [ width shrink
        , height (indicatorHeight |> px)
        , styleAttr "padding-left" "5px"
        , styleAttr "padding-right" "5px"
        , styleAttr "border-radius" "0px"
        , Background.color color
        ]
        (centered content)


insertionIndicator : Element msg
insertionIndicator =
    indicator createColor (indicatorIcon Icons.plus)


deletionIndicator : Element msg
deletionIndicator =
    indicator destroyColor (indicatorIcon Icons.x)


iterationText : String -> Bool -> Element msg
iterationText originName isHovered =
    let
        fontColor =
            if isHovered then
                "black"

            else
                "white"

        symbolText =
            el
                [ fontSize 18
                , styleAttr "color" fontColor
                , Font.bold
                , Font.family
                    [ Font.typeface "Open Sans"
                    , Font.sansSerif
                    ]
                ]
                (text "use")

        originText =
            el
                [ fontSize 16
                , styleAttr "color" fontColor
                , nameFontFamily
                , alignBottom
                ]
                (text originName)
    in
    row [ spacing 7 ] [ symbolText, originText ]


interactiveIndicator : Element.Color -> (Maybe Id -> msg) -> Maybe Id -> String -> Bool -> Element msg
interactiveIndicator color mkMsg originId originName isHovered =
    let
        backgroundColor =
            if isHovered then
                useColor |> Utils.Color.toElement

            else
                color

        baseIndicator =
            indicator backgroundColor (iterationText originName isHovered)
    in
    case originId of
        Just id ->
            el
                [ Events.onMouseEnter (mkMsg (Just id))
                , Events.onMouseLeave (mkMsg Nothing)
                ]
                baseIndicator

        Nothing ->
            baseIndicator


iterationIndicator : Maybe Id -> String -> Bool -> Element Msg
iterationIndicator =
    interactiveIndicator createColor HighlightOrigin


deiterationIndicator : Maybe Id -> String -> Bool -> Element Msg
deiterationIndicator =
    interactiveIndicator destroyColor HighlightOrigin


expansionIndicator : Element msg
expansionIndicator =
    indicator expandColor (indicatorIcon Icons.maximize2)


collapseIndicator : Element msg
collapseIndicator =
    indicator collapseColor (indicatorIcon Icons.minimize2)



-- Misc


fullPageTextMessage : String -> Element msg
fullPageTextMessage txt =
    el
        [ width fill
        , height fill
        , Background.color (rgb 1 1 1)
        ]
        (el
            [ centerX
            , centerY
            , fontSize 50
            ]
            (text txt)
        )
