module View.Route exposing (..)

import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, top)


type Route
    = Playground
    | Manual
    | NotFound String


routeParser : Parser (Route -> a) a
routeParser =
    let
        refl =
            s "scroller"
    in
    oneOf
        [ map Playground top
        , map Playground (top </> s "src" </> s "Main.elm") -- For use with elm reactor
        , map Playground (top </> s "index.html")
        , map Playground refl
        , map Playground (refl </> s "index.html")
        , map Manual (s "manual")
        , map Manual (refl </> s "manual")
        ]


fromUrl : Url.Url -> Route
fromUrl url =
    Maybe.withDefault (NotFound url.path) (parse routeParser url)
