module View.Widgets exposing (..)

import Color
import Css
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icons exposing (Icon)
import Html.Attributes
import Html.Styled exposing (fromUnstyled, toUnstyled)
import Html.Styled.Attributes as Attrs exposing (action, css)
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
            let
                iconColor =
                    if enabled then
                        style.iconColorEnabled

                    else
                        style.iconColorDisabled
            in
            Html.Styled.div
                [ css [ Css.color (iconColor |> Utils.Color.toCss) ] ]
                [ content |> layout [] |> fromUnstyled ]

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
    button
        style
        { action = action
        , title = title
        , content = icon |> Icons.withSize 30 |> Icons.toHtml [] |> Element.html
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
    { color : Color
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
                , Border.rounded buttonBorderRadius
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
                                |> Utils.Color.toHtmlAttr
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


indicatorHeight : Length
indicatorHeight =
    20 |> px


indicatorIcon : Icon -> Element msg
indicatorIcon icon =
    icon
        |> Icons.withSize 15
        |> Icons.toHtml [ Color.white |> Utils.Color.toHtmlAttr ]
        |> html


indicatorText : String -> Element msg
indicatorText txt =
    el
        [ Font.color (Color.white |> Utils.Color.toElement)
        , Font.size 14
        , Font.family
            [ Font.typeface "Open Sans"
            , Font.sansSerif
            ]
        ]
        (text txt)


indicator : Color -> Element msg -> Element msg
indicator color content =
    el
        [ width indicatorHeight
        , height indicatorHeight
        , Border.rounded 2
        , Background.color color
        ]
        content


insertionIndicator : Element msg
insertionIndicator =
    indicator introColor (indicatorIcon Icons.plus)


deletionIndicator : Element msg
deletionIndicator =
    indicator elimColor (indicatorIcon Icons.x)


iterationIndicator : String -> Element msg
iterationIndicator name =
    indicator introColor (indicatorText (":=" ++ name))


deiterationIndicator : String -> Element msg
deiterationIndicator name =
    indicator elimColor (indicatorText (":=" ++ name))


expansionIndicator : Element msg
expansionIndicator =
    indicator introColor (indicatorIcon Icons.maximize2)


collapseIndicator : Element msg
collapseIndicator =
    indicator introColor (indicatorIcon Icons.minimize2)



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
            , Font.size 50
            ]
            (text txt)
        )
