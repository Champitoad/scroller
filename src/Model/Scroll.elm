module Model.Scroll exposing (..)

import Model.Formula as Formula exposing (..)

import Utils.List

type alias Net
  = List Judgment

type alias Judgment
  = { metadata : Metadata
    , ident : Maybe VarIdent
    , justif : Justification
    , shape : Shape }

type alias VarIdent = String

type alias Justification
  = { self : Bool
    , from : Maybe VarIdent }

type alias Metadata
  = { grown : Bool }

type Shape
  = Formula Formula
  | Scroll ScrollData

type alias ScrollData
  = { interaction : Interaction
    , outloop : Net
    , inloops : List InloopData }

type alias Interaction
  = { opened : Maybe Int
    , closed : Maybe Int }

type alias InloopData
  = { ident : Maybe ConstrIdent
    , content : Net }

type alias ConstrIdent = String


isGrown : Judgment -> Bool
isGrown judgment =
  judgment.metadata.grown


commit : Judgment -> Judgment
commit judgment =
  let
    oldMetadata = judgment.metadata
    newMetadata = { oldMetadata | grown = False }
  in
  { judgment | metadata = newMetadata }
  

assume : Metadata -> Shape -> Judgment
assume metadata shape =
  { metadata = metadata
  , ident = Nothing
  , justif = { self = False, from = Nothing }
  , shape = shape }


mkFormula : Metadata -> Formula -> Judgment
mkFormula metadata statement =
  assume metadata (Formula statement)


mkRealFormula : Formula -> Judgment
mkRealFormula =
  mkFormula { grown = False }

f : Formula -> Judgment
f = mkRealFormula


mkFakeFormula : Formula -> Judgment
mkFakeFormula =
  mkFormula { grown = True }


mkInloop : Maybe ConstrIdent -> Net -> InloopData
mkInloop ident content =
  { ident = ident, content = content }

mkScroll : Metadata -> Net -> List Net -> Judgment
mkScroll metadata outloop inloops =
  assume metadata
    (Scroll
      { interaction = { opened = Nothing, closed = Nothing }
      , outloop = outloop
      , inloops = List.map (mkInloop Nothing) inloops })


mkRealScroll : Net -> List Net -> Judgment
mkRealScroll =
  mkScroll { grown = False }


mkFakeScroll : Net -> List Net -> Judgment
mkFakeScroll =
  mkScroll  { grown = True }


{-| Make a scroll from an outloop and a list of inloops.
    
    s [a"a", a"b"] [[a"c"], [a"d"]]
-}
s : Net -> List Net -> Judgment
s inloop outloops =
  mkRealScroll inloop outloops


mkNet : Metadata -> List Shape -> Net
mkNet metadata shapes =
  List.map (assume metadata) shapes


mkRealNet : List Shape -> Net
mkRealNet =
  mkNet { grown = False }


mkFakeNet : List Shape -> Net
mkFakeNet =
  mkNet { grown = True }


decompose : Formula -> List Judgment
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
    , ident : Maybe VarIdent
    , justif : Justification
    , interaction : Interaction }


type Zip
  = ZNet { left : Net, right : Net }
  | ZOutloop { scroll : ZScrollData, inloops : List InloopData }
  | ZInloop { scroll : ZScrollData
            , ident : Maybe ConstrIdent
            , outloop : Net
            , left : List InloopData
            , right : List InloopData }


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


mkZOutloop : ZScrollData -> List InloopData -> Zip
mkZOutloop scroll inloops =
  ZOutloop { scroll = scroll, inloops = inloops }


mkZInloop : ZScrollData -> Maybe ConstrIdent -> Net -> List InloopData -> List InloopData -> Zip
mkZInloop scroll ident outloop left right =
  ZInloop { scroll = scroll
         , ident = ident
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
       , ident = scroll.ident
       , justif = scroll.justif
       , shape = Scroll { interaction = scroll.interaction
                        , outloop = net
                        , inloops = inloops } }]

    ZInloop { scroll, ident, outloop, left, right } ->
      [{ metadata = scroll.metadata
       , ident = scroll.ident
       , justif = scroll.justif
       , shape = Scroll { interaction = scroll.interaction
                        , outloop = outloop
                        , inloops = left ++ { ident = ident, content = net } :: right } }]


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


isHypothesis : Judgment -> Zipper -> Bool
isHypothesis judgment zipper =
  List.member judgment (hypsZipper zipper)


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
    walkJudgment acc judgment path_ =
      case path_ of
        [] -> Just (List.reverse acc, [judgment])
        n :: tail ->
          case judgment.shape of
            Formula _ -> Nothing
            Scroll { interaction, outloop, inloops } ->
              let scroll =
                { metadata = judgment.metadata
                , ident = judgment.ident
                , justif = judgment.justif
                , interaction = interaction }
              in
              if n == 0 then
                let zOutloop = mkZOutloop scroll inloops in
                walkNet (zOutloop :: acc) outloop tail
              else
                case Utils.List.pivot (n - 1) inloops of
                  (l, inloop :: r) ->
                    let zInloop = mkZInloop scroll inloop.ident outloop l r in
                    walkNet (zInloop :: acc) inloop.content tail
                  _ ->
                    Nothing
    
    walkNet acc net_ path_ =
      case path_ of
        [] -> Just (List.reverse acc, net_)
        n :: tail ->
          case Utils.List.pivot (n - 1) net_ of
            (l, judgment :: r) ->
              walkJudgment (mkZNet l r :: acc) judgment tail
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
        
        inloopText index { ident, content } =
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
          case ident of
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


viewJudgmentText : Judgment -> String
viewJudgmentText { ident, justif, shape } =
  let
    identText =
      case ident of
        Just name -> name
        Nothing -> ""
    
    selfText = 
      if justif.self
      then "•"
      else ""
        
    fromText =
      case justif.from of
        Just name -> name ++ "/"
        Nothing -> ""
    
    shapeText =
      viewShapeText shape
  in
  fromText ++ selfText ++ identText ++ " : " ++ shapeText


viewNetText : Net -> String
viewNetText net =
  net
  |> List.map viewJudgmentText
  |> String.join ", "


viewZipperText : Zipper -> String
viewZipperText zipper =
  fillZipper [mkRealFormula (Formula.atom "□")] zipper
  |> List.map viewJudgmentText
  |> String.join ", "

logZipper : String -> Zipper -> String
logZipper msg zipper =
  zipper
  |> viewZipperText
  |> Debug.log msg

logNet : String -> Net -> String
logNet msg net =
  net
  |> List.map viewJudgmentText
  |> String.join ", "
  |> Debug.log msg


-- Examples


atom : String -> Judgment
atom name =
  mkRealFormula (Formula.atom name)


{-| Make an atomic formula from a string.
    
    a "foo" == atom "foo"
-}
a : String -> Judgment
a = atom


entails : Net -> Net -> Judgment
entails phi psi =
  mkRealScroll phi [psi]


yinyang : Judgment
yinyang =
  s[][[]]


identity : Judgment
identity =
  s[a"a"][[a"a"]]


testFlower : Judgment
testFlower =
  s[s[a"b"][[a"c"]]][[a"a",s[a"b"][[a"c"]]]]

{-| "[a, [a (b) ([b (c)], b)], [d (e)] (b, a) (c)]"
-}
bigFlower : Judgment
bigFlower =
  s[a"a",s[a"a"][[a"b"],[s[a"b"][[a"c"]],a"b"]],s[a"d"][[a"e"]]][[a"b",a"a"],[a"c"]]
 

modusPonensCurryfied : Judgment
modusPonensCurryfied =
  s[s[a"a"][[a"b"]]][[s[a"a"][[a"b"]]]]


notFalse : Judgment
notFalse =
  s[s[][]][]


criticalPair : Judgment
criticalPair =
  s[s[][[a"a"],[a"b"]]
   ,s[a"a"][[a"c"]]
   ,s[a"b"][[a"c"]]]
   [[a"c"]]


orElim : Judgment
orElim =
  mkRealFormula
    ( Implies
        ( And
          ( Implies (Formula.atom "a") (Formula.atom "c") )
          ( Implies (Formula.atom "b") (Formula.atom "c") ) )
        ( Implies
          ( Or (Formula.atom "a") (Formula.atom "b") )
          ( Formula.atom "c" ) ) )


orElimInvertible : Judgment
orElimInvertible =
  mkRealFormula
    ( Implies
        ( Implies
          ( Or (Formula.atom "a") (Formula.atom "b") )
          ( Formula.atom "c" ) )
        ( And
          ( Implies (Formula.atom "a") (Formula.atom "c") )
          ( Implies (Formula.atom "b") (Formula.atom "c") ) ) )


kreiselPutnam : Judgment
kreiselPutnam =
  mkRealFormula
    ( Implies
        ( Implies
            ( Not (Formula.atom "a") )
            ( Or (Formula.atom "b") (Formula.atom "c") ) )
        ( Or
            ( Implies (Not (Formula.atom "a")) (Formula.atom "b") )
            ( Implies (Not (Formula.atom "a")) (Formula.atom "c") ) ) )
