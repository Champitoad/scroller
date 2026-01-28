module Model.Mascarpone exposing (..)

import Model.Formula exposing (..)
import Model.Scroll exposing (..)


mascarpone : Formula
mascarpone =
    Atom
        (Image
            { src = "public/assets/img/mascarpone.png"
            , description = "mascarpone"
            }
        )


sugar : Formula
sugar =
    Atom
        (Image
            { src = "public/assets/img/sugar.png"
            , description = "sugar"
            }
        )


egg : Formula
egg =
    Atom
        (Image
            { src = "public/assets/img/egg.png"
            , description = "egg"
            }
        )


white : Formula
white =
    Atom
        (Image
            { src = "public/assets/img/white.png"
            , description = "white"
            }
        )


yolk : Formula
yolk =
    Atom
        (Image
            { src = "public/assets/img/yolk.png"
            , description = "yolk"
            }
        )


whiskedWhites : Formula
whiskedWhites =
    Atom
        (Image
            { src = "public/assets/img/whisked-whites.png"
            , description = "whisked whites"
            }
        )


yolkPaste : Formula
yolkPaste =
    Atom
        (Image
            { src = "public/assets/img/yolk-paste.png"
            , description = "yolk paste"
            }
        )


thickPaste : Formula
thickPaste =
    Atom
        (Image
            { src = "public/assets/img/thick-paste.png"
            , description = "thick paste"
            }
        )


mascarponeCream : Formula
mascarponeCream =
    Atom
        (Image
            { src = "public/assets/img/mascarpone-cream.png"
            , description = "mascarpone cream"
            }
        )


crack : OToken
crack =
    curl [ fo egg ] [ [ fo yolk ], [ fo white ] ]


whisk : OToken
whisk =
    curl [ fo white ] [ [ fo whiskedWhites ] ]


beat : OToken
beat =
    curl [ fo yolk, fo sugar ] [ [ fo yolkPaste ] ]


stir : OToken
stir =
    curl [ fo yolkPaste, fo mascarpone ] [ [ fo thickPaste ] ]


fold : OToken
fold =
    curl [ fo whiskedWhites, fo thickPaste ] [ [ fo mascarponeCream ] ]


mascarponeCreamRecipe : Net
mascarponeCreamRecipe =
    netOfStruct [ curl [ crack, whisk, beat, stir, fold ] [ [ curl [ fo egg, fo sugar, fo mascarpone ] [ [ fo mascarponeCream ] ] ] ] ]
