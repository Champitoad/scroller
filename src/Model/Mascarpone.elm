module Model.Mascarpone exposing (..)

import Model.Formula exposing (..)
import Model.Scroll exposing (..)

mascarpone : Formula
mascarpone =
  Atom (Image
  { src = "public/assets/img/mascarpone.png"
  , description = "mascarpone" })

sugar : Formula
sugar =
  Atom (Image
  { src = "public/assets/img/sugar.png"
  , description = "sugar" })


egg : Formula
egg =
  Atom (Image
  { src = "public/assets/img/egg.png"
  , description = "egg" })

white : Formula
white =
  Atom (Image
  { src = "public/assets/img/white.png"
  , description = "white" })

yolk : Formula
yolk =
  Atom (Image
  { src = "public/assets/img/yolk.png"
  , description = "yolk" })

whiskedWhites : Formula
whiskedWhites =
  Atom (Image
  { src = "public/assets/img/whisked-whites.png"
  , description = "whisked whites" })

yolkPaste : Formula
yolkPaste =
  Atom (Image
  { src = "public/assets/img/yolk-paste.png"
  , description = "yolk paste" })

thickPaste : Formula
thickPaste =
  Atom (Image
  { src = "public/assets/img/thick-paste.png"
  , description = "thick paste" })

mascarponeCream : Formula
mascarponeCream =
  Atom (Image
  { src = "public/assets/img/mascarpone-cream.png"
  , description = "mascarpone cream" })

crack : Val
crack =
  entails [f egg] [f yolk, f white]

whisk : Val
whisk =
  entails [f white] [f whiskedWhites]

beat : Val
beat =
  entails [f yolk, f sugar] [f yolkPaste]

stir : Val
stir =
  entails [f yolkPaste, f mascarpone] [f thickPaste]

fold : Val
fold =
  entails [f whiskedWhites, f thickPaste] [f mascarponeCream]


mascarponeCreamRecipe : Net
mascarponeCreamRecipe =
  [entails [crack, whisk, beat, stir, fold] [entails [f egg, f sugar, f mascarpone] [f mascarponeCream]]]