module Model.Scroll exposing (..)

import Model.Formula as Formula exposing (..)

import Utils.List
import Utils.Maybe
import Dict exposing (Dict)


-- Scroll structures


type alias Struct
  = List Node

type Node
  = NFormula Formula
  | NScroll { outloop : Struct
            , inloops : SCorolla }

type alias SCorolla = Dict Ident Struct

type alias Ident = String


-- Scroll nets


type alias Net
  = List Val

type alias Val
  = { metadata : Metadata
    , name : Maybe Ident
    , justif : Justification
    , shape : Shape }

type alias Metadata
  = { grown : Bool }

type alias Justification
  = { self : Bool
    , from : Maybe Ident }

type Shape
  = Formula Formula
  | Scroll ScrollData

type alias ScrollData
  = { interaction : Interaction
    , outloop : Net
    , inloops : Corolla }

type alias Interaction
  = { opened : Maybe Ident
    , closed : Maybe Ident }

type alias Corolla
  = Dict Ident Env

type alias Env
  = { metadata : Metadata
    , justif : Justification
    , content : Net }


type alias ScrollVal
  = { metadata : Metadata
    , name : Maybe Ident
    , justif : Justification
    , data : ScrollData }

valOfScroll : ScrollVal -> Val
valOfScroll { metadata, name, justif, data } =
  { metadata = metadata
  , name = name
  , justif = justif
  , shape = Scroll data }


-- Conversions between structures and nets


netFromStruct : Struct -> Net
netFromStruct =
  List.map valFromNode

valFromNode : Node -> Val
valFromNode node =
  let
    envFromInloop : Struct -> Env
    envFromInloop inloop =
      { metadata = { grown = False }
      , justif = assumption
      , content = netFromStruct inloop }

    shape =
      case node of
        NFormula formula ->
          Formula formula
        
        NScroll { outloop, inloops } ->
          Scroll { interaction = { opened = Nothing, closed = Nothing }
                 , outloop = netFromStruct outloop
                 , inloops = Dict.map (\_ -> envFromInloop) inloops }
  in
  { metadata = { grown = False }
  , name = Nothing
  , justif = assumption
  , shape = shape }


structFromNet : Net -> Struct
structFromNet net =
  List.map nodeFromVal net


nodeFromVal : Val -> Node
nodeFromVal { shape } =
  let
    inloopFromEnv : Env -> Struct
    inloopFromEnv { content } =
      structFromNet content
  in
  case shape of
    Formula formula ->
      NFormula formula
    
    Scroll { outloop, inloops } ->
      NScroll { outloop = structFromNet outloop
              , inloops = Dict.map (\_ -> inloopFromEnv) inloops }


-- Boundaries


premiss : Net -> Struct
premiss net =
  Debug.todo "Premiss computation not implemented yet."

conclusion : Net -> Struct
conclusion net =
  Debug.todo "Conclusion computation not implemented yet."


-- Helper functions


assumption : Justification
assumption =
  { self = False, from = Nothing }


isGrownVal : Val -> Bool
isGrownVal val =
  val.metadata.grown

isGrownEnv : Env -> Bool
isGrownEnv env =
  env.metadata.grown


commitVal : Val -> Val
commitVal val =
  case (val.shape, val.metadata.grown) of
    (Scroll { interaction, outloop, inloops }, False) ->
      { val | shape = Scroll { interaction = interaction
                             , outloop = List.map commitVal outloop
                             , inloops = Dict.map (\_ -> commitEnv) inloops } }
 
    _ ->
      let
        oldMetadata = val.metadata
        newMetadata = { oldMetadata | grown = False }
      in
      { val | metadata = newMetadata }

commitEnv : Env -> Env
commitEnv env =
  if env.metadata.grown then
    let
      oldMetadata = env.metadata
      newMetadata = { oldMetadata | grown = False }
    in
    { env | metadata = newMetadata }
  else
    { env | content = List.map commitVal env.content }
  

mkShape : Metadata -> Shape -> Val
mkShape metadata shape =
  { metadata = metadata
  , name = Nothing
  , justif = assumption
  , shape = shape }


mkFormula : Metadata -> Formula -> Val
mkFormula metadata statement =
  mkShape metadata (Formula statement)


mkRealFormula : Formula -> Val
mkRealFormula =
  mkFormula { grown = False }

f : Formula -> Val
f = mkRealFormula


mkFakeFormula : Formula -> Val
mkFakeFormula =
  mkFormula { grown = True }


mkEnv : Metadata -> Net -> Env
mkEnv metadata content =
  { metadata = metadata
  , justif = assumption
  , content = content }

mkScroll : Metadata -> Net -> List (Ident, Net) -> Val
mkScroll metadata outloop inloopsAssoc =
  let
    inloops =
      inloopsAssoc |>
      List.map (\(k, content) -> (k, mkEnv { grown = False } content)) |>
      Dict.fromList

    scroll =
      Scroll { interaction = { opened = Nothing, closed = Nothing }
             , outloop = outloop
             , inloops = inloops }
  in
  mkShape metadata scroll


mkRealScroll : Net -> List (Ident, Net) -> Val
mkRealScroll =
  mkScroll { grown = False }


mkFakeScroll : Net -> List (Ident, Net) -> Val
mkFakeScroll =
  mkScroll  { grown = True }


{-| Make a scroll from an outloop and a list of inloops.
    
    s [a"a", a"b"] [[a"c"], [a"d"]]
-}
s : Net -> List Net -> Val
s inloop outloopsContents =
  let
    outloops =
      outloopsContents |>
      List.indexedMap
        (\i content -> ("Branch " ++ String.fromInt i, content))
  in
  mkRealScroll inloop outloops


mkNet : Metadata -> List Shape -> Net
mkNet metadata =
  List.map (mkShape metadata)


mkRealNet : List Shape -> Net
mkRealNet =
  mkNet { grown = False }


mkFakeNet : List Shape -> Net
mkFakeNet =
  mkNet { grown = True }


decompose : Formula -> List Val
decompose formula = 
  case formula of
    Atom _ ->
      [mkRealFormula formula]

    Truth ->
      []
    
    Falsity ->
      [mkRealScroll (mkRealNet []) []]
    
    And f1 f2 ->
      [mkRealFormula f1, mkRealFormula f2]
    
    Or f1 f2 ->
      [ mkRealScroll
          ( mkRealNet [] )
          [ ("Left", mkRealNet [Formula f1])
          , ("Right", mkRealNet [Formula f2]) ] ]
    
    Implies f1 f2 ->
      [ mkRealScroll
          ( mkRealNet [Formula f1] )
          [ ("Return", mkRealNet [Formula f2]) ] ]
    
    Not f1 ->
      [ mkRealScroll (mkRealNet [Formula f1]) [] ]


-- Zippers


type alias ZScrollData
  = { metadata : Metadata
    , name : Maybe Ident
    , justif : Justification
    , interaction : Interaction }


type Zip
  = ZNet { left : Net, right : Net }
  | ZOutloop { scroll : ZScrollData, inloops : Corolla }
  | ZInloop { scroll : ZScrollData
            , outloop : Net
            , neighbors : Corolla
            , metadata : Metadata
            , justif : Justification
            , id : Ident }


{- We encode zippers as lists, where the head is the innermost context.
-}
type alias Zipper
  = List Zip


isGrownZip : Zip -> Bool
isGrownZip zip =
  case zip of
    ZOutloop { scroll } ->
      scroll.metadata.grown
    ZInloop { scroll } ->
      scroll.metadata.grown
    _ ->
      False


isGrownZipper : Zipper -> Bool
isGrownZipper zipper =
  Utils.List.exists isGrownZip zipper


mkZNet : Net -> Net -> Zip
mkZNet left right =
  ZNet { left = left, right = right }


mkZOutloop : ZScrollData -> Corolla -> Zip
mkZOutloop scroll inloops =
  ZOutloop { scroll = scroll, inloops = inloops }


mkZInloop : ZScrollData -> Net -> Corolla ->
            Metadata -> Justification -> Ident -> Zip
mkZInloop scroll outloop neighbors metadata justif id =
  ZInloop { scroll = scroll
          , outloop = outloop
          , neighbors = neighbors
          , metadata = metadata
          , justif = justif
          , id = id }


fillZip : Zip -> Net -> Net
fillZip zip net =
  case zip of
    ZNet { left, right } ->
      left ++ net ++ right

    ZOutloop { scroll, inloops } ->
      [{ metadata = scroll.metadata
       , name = scroll.name
       , justif = scroll.justif
       , shape = Scroll { interaction = scroll.interaction
                        , outloop = net
                        , inloops = inloops } }]

    ZInloop { scroll, outloop, neighbors, metadata, justif, id } ->
      [{ metadata = scroll.metadata
       , name = scroll.name
       , justif = scroll.justif
       , shape =
           let
             env =
               { metadata = metadata
               , justif = justif
               , content = net }
           in
           Scroll { interaction = scroll.interaction
                  , outloop = outloop
                  , inloops = Dict.insert id env neighbors } }]


fillZipper : Net -> Zipper -> Net
fillZipper =
  List.foldl fillZip


hypsZip : Zip -> Net
hypsZip zip =
  case zip of
    ZNet { left, right } ->
      left ++ right

    ZOutloop _ ->
      []
    
    ZInloop { outloop } ->
      outloop


hypsZipper : Zipper -> Net
hypsZipper zipper =
  List.foldl (\zip acc -> hypsZip zip ++ acc) [] zipper


isHypothesis : Val -> Zipper -> Bool
isHypothesis val zipper =
  List.member val (hypsZipper zipper)


type Polarity
  = Pos
  | Neg


invert : Polarity -> Polarity
invert polarity =
  case polarity of
    Pos -> Neg
    Neg -> Pos


polarityOf : Zipper -> Polarity
polarityOf zipper =
  case zipper of
    [] ->
      Pos

    ZNet _ :: parent ->
      polarityOf parent

    ZOutloop _ :: parent ->
      invert (polarityOf parent)

    ZInloop _ :: parent ->
      polarityOf parent


{- `spans src tgt` is true when location `tgt` is in scope of location `src`, in the sense of
   Peirce's endoporeutic. 
-}
spans : Zipper -> Zipper -> Bool
spans src tgt =
  let lca = Utils.List.longestCommonSuffix src tgt in
  case src of
    -- Value location
    ZNet _ :: parent ->
      lca == parent ||       -- Cross-pollination
      case parent of
        ZOutloop _ :: grandParent ->
          lca == grandParent -- Self-pollination
        _ ->
          False
          
    -- Env location 
    ZInloop _ :: parent ->
      lca == parent &&
      -- We enforce the two inloops to be attached to the same outloop (might be relaxed in a later
      -- implementation, e.g. with vertically generalized scrolls or classical logic version)
      List.length src == List.length tgt
    
    _ ->
      False


-- Intro/elim


type alias Context
  = { zipper : Zipper,
      polarity : Polarity }


introducedJustif : Polarity -> Justification -> Bool
introducedJustif pol justif =
  case pol of
    Pos ->
      case justif.from of
        Just _ -> True
        Nothing -> False

    Neg ->
      justif.self

eliminatedJustif : Polarity -> Justification -> Bool
eliminatedJustif pol justif =
  introducedJustif (invert pol) justif


introducedZip : Polarity -> Zip -> Bool
introducedZip pol zip =
  case zip of
    ZNet _ ->
      False

    ZOutloop { scroll } ->
      introducedJustif pol scroll.justif

    ZInloop { scroll, justif } ->
      introducedJustif pol scroll.justif ||
      introducedJustif (invert pol) justif

eliminatedZip : Polarity -> Zip -> Bool
eliminatedZip pol zip =
  introducedZip (invert pol) zip


introducedContext : Context -> Bool
introducedContext { zipper, polarity } =
  let
    aux pol zipper_ =
      case zipper_ of
        [] ->
          False

        zip :: parent ->
          introducedZip pol zip ||
          let
            newPol =
              case zip of
                ZOutloop _ -> invert pol
                _ -> pol
          in
          aux newPol parent
  in
  aux polarity zipper

eliminatedContext : Context -> Bool
eliminatedContext { zipper, polarity } =
  introducedContext { zipper = zipper, polarity = invert polarity }


introducedVal : Context -> Val -> Bool
introducedVal ctx val =
  let
    isOpened =
      case val.shape of
        Scroll { interaction } ->
          Utils.Maybe.isSomething interaction.opened
        _ ->
          False
  in
  introducedJustif ctx.polarity val.justif ||
  isOpened ||
  introducedContext ctx

eliminatedVal : Context -> Val -> Bool
eliminatedVal ctx val =
  let
    isClosed =
      case val.shape of
        Scroll { interaction } ->
          Utils.Maybe.isSomething interaction.closed
        _ ->
          False
  in
  eliminatedJustif ctx.polarity val.justif ||
  isClosed ||
  eliminatedContext ctx


introducedScrollVal : Context -> ScrollVal -> Bool
introducedScrollVal ctx scroll =
  introducedVal ctx (valOfScroll scroll)

eliminatedScrollVal : Context -> ScrollVal -> Bool
eliminatedScrollVal ctx scroll =
  eliminatedVal ctx (valOfScroll scroll)


introducedEnv : Context -> ScrollVal -> Ident -> Env -> Bool
introducedEnv ctx scroll id { justif } =
  introducedJustif (invert ctx.polarity) justif ||
  scroll.data.interaction.opened == Just id ||
  introducedJustif ctx.polarity scroll.justif ||
  introducedContext ctx

eliminatedEnv : Context -> ScrollVal -> Ident -> Env -> Bool
eliminatedEnv ctx scroll id { justif } =
  eliminatedJustif (invert ctx.polarity) justif ||
  scroll.data.interaction.closed == Just id ||
  eliminatedJustif ctx.polarity scroll.justif ||
  eliminatedContext ctx


introducedArea : Context -> Bool
introducedArea ctx =
  case ctx.zipper of
    [] ->
      False
    
    zip :: parent ->
      case zip of
        ZOutloop { scroll } ->
          Utils.Maybe.isSomething scroll.interaction.opened ||
          introducedJustif (invert ctx.polarity) scroll.justif ||
          introducedArea { zipper = parent, polarity = invert ctx.polarity }

        ZInloop { scroll } ->
          introducedJustif ctx.polarity scroll.justif ||
          introducedArea { zipper = parent, polarity = ctx.polarity }
        
        ZNet _ ->
          introducedArea { zipper = parent, polarity = ctx.polarity }


eliminatedArea : Context -> Bool
eliminatedArea ctx =
  case ctx.zipper of
    [] ->
      False
    
    zip :: parent ->
      case zip of
        ZOutloop { scroll } ->
          Utils.Maybe.isSomething scroll.interaction.closed ||
          eliminatedJustif (invert ctx.polarity) scroll.justif ||
          eliminatedArea { zipper = parent, polarity = invert ctx.polarity }

        ZInloop { scroll } ->
          eliminatedJustif ctx.polarity scroll.justif ||
          eliminatedArea { zipper = parent, polarity = ctx.polarity }
        
        ZNet _ ->
          eliminatedArea { zipper = parent, polarity = ctx.polarity }


-- Paths


-- {-| Paths are a more abstract version of zippers where we only remember the
--     position as an integer, instead of the full surrounding context.
-- -}
-- type alias Path
--   = List Int


-- {-| A zipper can be reconstructed by "walking down" a path in a net. The path
--     does not always denote a valid branch of the net, thus this operation is
--     partial.
-- -}
-- walk : Net -> Path -> Maybe (Context, Net)
-- walk net path =
--   let
--     walkVal : List Zip -> Polarity -> Val -> Path -> Maybe (Context, Net)
--     walkVal acc pol val path_ =
--       case path_ of
--         [] -> Just ({ zipper = List.reverse acc, polarity = pol }, [val])
--         n :: tail ->
--           case val.shape of
--             Formula _ -> Nothing
--             Scroll { interaction, outloop, inloops } ->
--               let
--                 scroll =
--                   { metadata = val.metadata
--                   , name = val.name
--                   , justif = val.justif
--                   , interaction = interaction }
--               in
--               if n == 0 then
--                 let zOutloop = mkZOutloop scroll inloops in
--                 walkNet (zOutloop :: acc) (invert pol) outloop tail
--               else
--                 case Utils.List.pivot (n - 1) inloops of
--                   (l, { metadata, arg, content } :: r) ->
--                     let zInloop = mkZInloop scroll metadata arg outloop l r in
--                     walkNet (zInloop :: acc) pol content tail
--                   _ ->
--                     Nothing
    
--     walkNet acc pol net_ path_ =
--       case path_ of
--         [] -> Just ({ zipper = List.reverse acc, polarity = pol }, net_)
--         n :: tail ->
--           case Utils.List.pivot (n - 1) net_ of
--             (l, val :: r) ->
--               walkVal (mkZNet l r :: acc) pol val tail
--             _ ->
--               Nothing
--   in
--   walkNet [] Pos net path


-- -- Also there is a forgetful functor from zippers to paths


-- zipToInt : Zip -> Int
-- zipToInt zip =
--   case zip of
--     ZNet { left } ->
--       List.length left
--     ZOutloop _ ->
--       0
--     ZInloop { left } ->
--       1 + List.length left


-- zipperToPath : Zipper -> Path
-- zipperToPath =
--   List.map zipToInt


-- String representation


viewJustifText : Justification -> Ident -> String
viewJustifText { self, from } id =
  let
    selfText = 
      if self then "•" else ""
        
    fromText =
      case from of
        Just name -> name ++ "/"
        Nothing -> ""
  in
  fromText ++ selfText ++ id


viewShapeText : Shape -> String
viewShapeText shape =
  case shape of
    Formula formula ->
      Formula.toString formula
    
    Scroll { interaction, outloop, inloops } ->
      let
        outloopText =
          viewNetText outloop
        
        inloopText (id, { justif, content }) =
          let
            (isOpened, isClosed) =
              case (interaction.opened, interaction.closed) of
                (Just o, Just c) -> (id == o, id == c)
                (Just o, Nothing) -> (id == o, False)
                (Nothing, Just c) -> (False, id == c)
                (Nothing, Nothing) -> (False, False)

            openBracket = if isOpened then "<(" else "("
            closeBracket = if isClosed then ")>" else ")"

            contentText = openBracket ++ viewNetText content ++ closeBracket
          in
          viewJustifText justif id ++ " :: " ++ contentText

        inloopsText =
          inloops
          |> Dict.toList
          |> List.map inloopText
          |> String.join "; "
      in
      "[" ++ outloopText ++ " " ++ inloopsText ++ "]"


viewValText : Val -> String
viewValText { name, justif, shape } =
  let
    nameText =
      case name of
        Just id -> id
        Nothing -> ""
      
    shapeText =
      viewShapeText shape
  in
  viewJustifText justif nameText ++ " : " ++ shapeText


viewNetText : Net -> String
viewNetText net =
  net
  |> List.map viewValText
  |> String.join ", "


viewZipperText : Zipper -> String
viewZipperText zipper =
  fillZipper [mkRealFormula (Formula.atom "□")] zipper
  |> List.map viewValText
  |> String.join ", "

logZipper : String -> Zipper -> String
logZipper msg zipper =
  zipper
  |> viewZipperText
  |> Debug.log msg

logNet : String -> Net -> String
logNet msg net =
  net
  |> List.map viewValText
  |> String.join ", "
  |> Debug.log msg


-- Examples


atom : String -> Val
atom name =
  mkRealFormula (Formula.atom name)


{-| Make an atomic formula from a string.
    
    a "foo" == atom "foo"
-}
a : String -> Val
a = atom


entails : Net -> Net -> Val
entails phi psi =
  mkRealScroll phi [("Return", psi)]


yinyang : Val
yinyang =
  s[][[]]


identity : Val
identity =
  s[a"a"][[a"a"]]


testFlower : Val
testFlower =
  s[s[a"b"][[a"c"]]][[a"a",s[a"b"][[a"c"]]]]

{-| "[a, [a (b) ([b (c)], b)], [d (e)] (b, a) (c)]"
-}
bigFlower : Val
bigFlower =
  s[a"a",s[a"a"][[a"b"],[s[a"b"][[a"c"]],a"b"]],s[a"d"][[a"e"]]][[a"b",a"a"],[a"c"]]
 

modusPonensCurryfied : Val
modusPonensCurryfied =
  s[s[a"a"][[a"b"]]][[s[a"a"][[a"b"]]]]


notFalse : Val
notFalse =
  s[s[][]][]


criticalPair : Val
criticalPair =
  s[s[][[a"a"],[a"b"]]
   ,s[a"a"][[a"c"]]
   ,s[a"b"][[a"c"]]]
   [[a"c"]]


orElim : Val
orElim =
  mkRealFormula
    ( Implies
        ( And
          ( Implies (Formula.atom "a") (Formula.atom "c") )
          ( Implies (Formula.atom "b") (Formula.atom "c") ) )
        ( Implies
          ( Or (Formula.atom "a") (Formula.atom "b") )
          ( Formula.atom "c" ) ) )


orElimInvertible : Val
orElimInvertible =
  mkRealFormula
    ( Implies
        ( Implies
          ( Or (Formula.atom "a") (Formula.atom "b") )
          ( Formula.atom "c" ) )
        ( And
          ( Implies (Formula.atom "a") (Formula.atom "c") )
          ( Implies (Formula.atom "b") (Formula.atom "c") ) ) )


kreiselPutnam : Val
kreiselPutnam =
  mkRealFormula
    ( Implies
        ( Implies
            ( Not (Formula.atom "a") )
            ( Or (Formula.atom "b") (Formula.atom "c") ) )
        ( Or
            ( Implies (Not (Formula.atom "a")) (Formula.atom "b") )
            ( Implies (Not (Formula.atom "a")) (Formula.atom "c") ) ) )
