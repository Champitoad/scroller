module Model.Scroll exposing (..)

import Model.Formula as Formula exposing (..)

import Utils.List


-- Scroll structures


type alias Struct
  = List Node

type Node
  = NFormula Formula
  | NScroll { outloop : Struct
            , inloops : List Struct }


-- Scroll nets


type alias Net
  = List Val

type alias Val
  = { metadata : Metadata
    , arg : Argument
    , shape : Shape }

type alias Argument
  = { name : Maybe Ident
    , justif : Justification }

type alias Ident = String

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
    , inloops : List Env }

type alias Interaction
  = { opened : Maybe Int
    , closed : Maybe Int }

type alias Env
  = { metadata : Metadata
    , arg : Argument
    , content : Net }


-- Conversions between structures and nets


assumption : Justification
assumption =
  { self = False, from = Nothing }

assume : Maybe Ident -> Argument
assume name =
  { name = name
  , justif = assumption }


netFromStruct : Struct -> Net
netFromStruct struct =
  List.map valFromNode struct

valFromNode : Node -> Val
valFromNode node =
  let
    envFromInloop : Struct -> Env
    envFromInloop inloop =
      { metadata = { grown = False}
      , arg = assume Nothing
      , content = netFromStruct inloop }

    shape =
      case node of
        NFormula formula ->
          Formula formula
        
        NScroll { outloop, inloops } ->
          Scroll { interaction = { opened = Nothing, closed = Nothing }
                 , outloop = netFromStruct outloop
                 , inloops = List.map envFromInloop inloops }
  in
  { metadata = { grown = False }
  , arg = assume Nothing
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
              , inloops = List.map inloopFromEnv inloops }


-- Helper functions


isGrownVal : Val -> Bool
isGrownVal val =
  val.metadata.grown

isGrownEnv : Env -> Bool
isGrownEnv env =
  env.metadata.grown


commitVal : Val -> Val
commitVal val =
  let
    oldMetadata = val.metadata
    newMetadata = { oldMetadata | grown = False }
  in
  { val | metadata = newMetadata }

commitEnv : Env -> Env
commitEnv env =
  let
    oldMetadata = env.metadata
    newMetadata = { oldMetadata | grown = False }
  in
  { env | metadata = newMetadata }
  

mkShape : Metadata -> Shape -> Val
mkShape metadata shape =
  { metadata = metadata
  , arg = assume Nothing
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


mkInloop : Metadata -> Maybe Ident -> Net -> Env
mkInloop metadata ident content =
  { metadata = metadata
  , arg = assume ident
  , content = content }

mkScroll : Metadata -> Net -> List Net -> Val
mkScroll metadata outloop inloops =
  let
    scroll =
      Scroll { interaction = { opened = Nothing, closed = Nothing }
             , outloop = outloop
             , inloops = List.map (mkInloop metadata Nothing) inloops }
  in
  mkShape metadata scroll


mkRealScroll : Net -> List Net -> Val
mkRealScroll =
  mkScroll { grown = False }


mkFakeScroll : Net -> List Net -> Val
mkFakeScroll =
  mkScroll  { grown = True }


{-| Make a scroll from an outloop and a list of inloops.
    
    s [a"a", a"b"] [[a"c"], [a"d"]]
-}
s : Net -> List Net -> Val
s inloop outloops =
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
          [ mkRealNet [Formula f1]
          , mkRealNet [Formula f2] ] ]
    
    Implies f1 f2 ->
      [ mkRealScroll
          ( mkRealNet [Formula f1] )
          [ mkRealNet [Formula f2] ] ]
    
    Not f1 ->
      [ mkRealScroll (mkRealNet [Formula f1]) [] ]


-- Zippers


type alias ZScrollData
  = { metadata : Metadata
    , arg : Argument
    , interaction : Interaction }


type Zip
  = ZNet { left : Net, right : Net }
  | ZOutloop { scroll : ZScrollData, inloops : List Env }
  | ZInloop { scroll : ZScrollData
            , metadata : Metadata
            , arg : Argument
            , outloop : Net
            , left : List Env
            , right : List Env }


type alias Zipper
  = List Zip


-- We encode zippers as lists, where the head is the innermost context


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


mkZOutloop : ZScrollData -> List Env -> Zip
mkZOutloop scroll inloops =
  ZOutloop { scroll = scroll, inloops = inloops }


mkZInloop : ZScrollData -> Metadata -> Argument -> 
            Net -> List Env -> List Env -> Zip
mkZInloop scroll metadata arg outloop left right =
  ZInloop { scroll = scroll
          , metadata = metadata
          , arg = arg
          , outloop = outloop
          , left = left
          , right = right }


fillZip : Zip -> Net -> Net
fillZip zip net =
  case zip of
    ZNet { left, right } ->
      left ++ net ++ right

    ZOutloop { scroll, inloops } ->
      [{ metadata = scroll.metadata
       , arg = scroll.arg
       , shape = Scroll { interaction = scroll.interaction
                        , outloop = net
                        , inloops = inloops } }]

    ZInloop { scroll, metadata, arg, outloop, left, right } ->
      [{ metadata = scroll.metadata
       , arg = scroll.arg
       , shape =
           let
             inloop =
               { metadata = metadata
               , arg = arg
               , content = net }
           in
           Scroll { interaction = scroll.interaction
                    , outloop = outloop
                    , inloops = left ++ inloop :: right } }]


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


justifies : Zipper -> Zipper -> Bool
justifies source destination =
  let lca = Utils.List.longestCommonSuffix source destination in
  polarityOf source == polarityOf destination &&
  case source of
    -- Self pollination
    ZNet _ :: (ZOutloop _ :: grandParent as parent) ->
      lca == grandParent ||
      lca == parent

    -- Wind pollination
    ZNet _ :: parent ->
      lca == parent
    
    _ ->
      False


type alias Context
  = { zipper : Zipper,
      polarity : Polarity }


{-| Paths are a more abstract version of zippers where we only remember the
    position as an integer, instead of the full surrounding context.
-}
type alias Path
  = List Int


{-| A zipper can be reconstructed by "walking down" a path in a net. The path
    does not always denote a valid branch of the net, thus this operation is
    partial.
-}
walk : Net -> Path -> Maybe (Zipper, Net)
walk net path =
  let
    walkVal acc val path_ =
      case path_ of
        [] -> Just (List.reverse acc, [val])
        n :: tail ->
          case val.shape of
            Formula _ -> Nothing
            Scroll { interaction, outloop, inloops } ->
              let
                scroll =
                  { metadata = val.metadata
                  , arg = val.arg
                  , interaction = interaction }
              in
              if n == 0 then
                let zOutloop = mkZOutloop scroll inloops in
                walkNet (zOutloop :: acc) outloop tail
              else
                case Utils.List.pivot (n - 1) inloops of
                  (l, { metadata, arg, content } :: r) ->
                    let zInloop = mkZInloop scroll metadata arg outloop l r in
                    walkNet (zInloop :: acc) content tail
                  _ ->
                    Nothing
    
    walkNet acc net_ path_ =
      case path_ of
        [] -> Just (List.reverse acc, net_)
        n :: tail ->
          case Utils.List.pivot (n - 1) net_ of
            (l, judgment :: r) ->
              walkVal (mkZNet l r :: acc) judgment tail
            _ ->
              Nothing
  in
  walkNet [] net path


-- Also there is a forgetful functor from zippers to paths


zipToInt : Zip -> Int
zipToInt zip =
  case zip of
    ZNet { left } ->
      List.length left
    ZOutloop _ ->
      0
    ZInloop { left } ->
      1 + List.length left


zipperToPath : Zipper -> Path
zipperToPath =
  List.map zipToInt


-- String representation


viewShapeText : Shape -> String
viewShapeText shape =
  case shape of
    Formula formula ->
      Formula.toString formula
    
    Scroll { interaction, outloop, inloops } ->
      let
        outloopText =
          viewNetText outloop
        
        inloopText index { arg, content } =
          let
            (isOpened, isClosed) =
              case (interaction.opened, interaction.closed) of
                (Just o, Just c) -> (index == o, index == c)
                (Just o, Nothing) -> (index == o, False)
                (Nothing, Just c) -> (False, index == c)
                (Nothing, Nothing) -> (False, False)

            openBracket = if isOpened then "<(" else "("
            closeBracket = if isClosed then ")>" else ")"

            contentText = openBracket ++ viewNetText content ++ closeBracket
          in
          case arg.name of
            Just name ->
              name ++ " :: " ++ contentText
            Nothing ->
              contentText

        inloopsText =
          inloops
          |> List.indexedMap inloopText
          |> String.join "; "
      in
      "[" ++ outloopText ++ " " ++ inloopsText ++ "]"


viewValText : Val -> String
viewValText { arg, shape } =
  let
    identText =
      case arg.name of
        Just name -> name
        Nothing -> ""
    
    selfText = 
      if arg.justif.self
      then "•"
      else ""
        
    fromText =
      case arg.justif.from of
        Just name -> name ++ "/"
        Nothing -> ""
    
    shapeText =
      viewShapeText shape
  in
  fromText ++ selfText ++ identText ++ " : " ++ shapeText


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
  mkRealScroll phi [psi]


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
