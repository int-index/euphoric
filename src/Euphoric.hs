module Euphoric where

import Prelude.Experimental
import Data.Kind (Type, Constraint)
import Data.Proxy
import GHC.TypeLits
import Control.Monad.Trans.State
import Language.Haskell.TH (Pat, TExp, Exp)
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Ppr as TH.Ppr
import qualified Language.Haskell.TH.PprLib as TH.Ppr
import MiniQ

type KPairs = List (Tuple2 Symbol Type)

type Grammar :: k -> (Type -> Type) -> KPairs -> Type
data Grammar tok m prods = MkGrammar (GrammarBuilder tok m prods prods)

type GrammarBuilder :: k -> (Type -> Type) -> KPairs -> KPairs -> Type
data GrammarBuilder tok m allProds prods where
  EndOfGrammar :: GrammarBuilder tok m allProds []
  GrammarCons ::
    KnownSymbol sym =>
    Proxy sym ->
    Prod tok m allProds ty ->
    GrammarBuilder tok m allProds prods ->
    GrammarBuilder tok m allProds ((sym, ty) : prods)

type Symbols :: List Symbol -> Type
data Symbols xs where
  SymbolsNil :: Symbols []
  SymbolsCons :: KnownSymbol x => Proxy x -> Symbols xs -> Symbols (x : xs)

type Syms :: List Symbol -> Constraint
class Syms xs where
  symbols :: Symbols xs
instance Syms [] where
  symbols = SymbolsNil
instance (KnownSymbol x, Syms xs) => Syms (x : xs) where
  symbols = SymbolsCons Proxy symbols

symbolsVal :: Symbols xs -> List String
symbolsVal SymbolsNil = []
symbolsVal (SymbolsCons symProxy syms) = symbolVal symProxy : symbolsVal syms

type Repr :: KPairs -> Symbol -> Type
type family Repr prods sym where
  Repr [] sym = TypeError ('Text sym ':<>: 'Text " is not a symbol of the grammar")
  Repr ((sym, ty):_) sym = ty
  Repr (_:prods) sym = Repr prods sym

type WithReprs :: KPairs -> List Symbol -> Type -> Type
type family WithReprs allProds syms ty where
  WithReprs allProds [] ty = ty
  WithReprs allProds (sym:syms) ty =
    Repr allProds sym -> WithReprs allProds syms ty

type Rule :: KPairs -> (Type -> Type) -> Type -> Type
data Rule allProds m ty =
  forall syms. MkRule  (Symbols syms) (TExp (WithReprs allProds syms ty)) |
  forall syms. MkRuleM (Symbols syms) (TExp (WithReprs allProds syms (m ty)))

type Prod :: k -> (Type -> Type) -> KPairs -> Type -> Type
data Prod tok m allProds ty =
  MkTerm Pat |
  MkNonTerm (List (Rule allProds m ty))

(==>) ::
  forall syms ->
  forall allProds m ty.
  Syms syms =>
  TH.Code MiniQ (WithReprs allProds syms ty) ->
  State (List (Rule allProds m ty)) Unit
syms ==> reduction = modify (MkRule (symbols @syms) (runMiniQ (TH.examineCode reduction)):)

(==>%) ::
  forall syms ->
  forall allProds m ty.
  Syms syms =>
  TH.Code MiniQ (WithReprs allProds syms (m ty)) ->
  State (List (Rule allProds m ty)) Unit
syms ==>% reduction = modify (MkRuleM (symbols @syms) (runMiniQ (TH.examineCode reduction)):)

tm ::
  forall sym ->
  forall tok m allProds prods.
  KnownSymbol sym =>
  MiniQ Pat ->
  GrammarBuilder tok m allProds prods ->
  GrammarBuilder tok m allProds ((sym, tok) : prods)
tm _ f = GrammarCons Proxy (MkTerm (runMiniQ f))

nt ::
  forall sym ty ->
  forall tok m allProds prods.
  KnownSymbol sym =>
  State (List (Rule allProds m ty)) Unit ->
  GrammarBuilder tok m allProds prods ->
  GrammarBuilder tok m allProds ((sym, ty) : prods)
nt _ _ s = GrammarCons Proxy (MkNonTerm (execState s []))

data UGrammar = UGrammar (List (Tuple2 String Pat)) (List (Tuple2 String (List URule)))

data URule = URule (List String) Exp | URuleM (List String) Exp

toUGrammar :: Grammar tok m prods -> UGrammar
toUGrammar (MkGrammar g) = toUGrammar' g

toUGrammar' :: GrammarBuilder tok m allProds prods -> UGrammar
toUGrammar' EndOfGrammar = UGrammar [] []
toUGrammar' (GrammarCons symProxy p g) =
  let sym = symbolVal symProxy
      UGrammar utms unts = toUGrammar' g
  in case p of
    MkTerm pat -> UGrammar ((sym,pat):utms) unts
    MkNonTerm rules ->
      let urules = map toURule rules
      in UGrammar utms ((sym,urules):unts)

toURule :: Rule allProds m ty -> URule
toURule (MkRule syms texp) =
  URule (symbolsVal syms) (TH.unType texp)
toURule (MkRuleM syms texp) =
  URuleM (symbolsVal syms) (TH.unType texp)

pprUSym :: String -> TH.Ppr.Doc
pprUSym = TH.Ppr.quotes . TH.Ppr.text

pprUGrammar :: UGrammar -> TH.Ppr.Doc
pprUGrammar (UGrammar utms unts) =
  TH.Ppr.hang (TH.Ppr.text "%token") 2
    (TH.Ppr.vcat (map pprUTerm utms))
  TH.Ppr.$$
  TH.Ppr.text "%%" TH.Ppr.$$
    TH.Ppr.vcat (map pprUNonTerm unts)

pprUTerm :: Tuple2 String Pat -> TH.Ppr.Doc
pprUTerm (s, pat) = pprUSym s TH.Ppr.<+> TH.Ppr.braces (TH.Ppr.ppr pat)

pprUNonTerm :: Tuple2 String (List URule) -> TH.Ppr.Doc
pprUNonTerm (_, []) = error "pprUNonTerm: empty rules"
pprUNonTerm (s, rule1:rules) =
  TH.Ppr.hang (pprUSym s) 2 $
    TH.Ppr.char ':' TH.Ppr.<+> pprURule rule1
    TH.Ppr.$$
    TH.Ppr.vcat [TH.Ppr.char '|' TH.Ppr.<+> pprURule ruleN | ruleN <- rules]

pprURule :: URule -> TH.Ppr.Doc
pprURule (URule ss e) =
  TH.Ppr.hsep (map pprUSym ss) TH.Ppr.<+>
  TH.Ppr.text "{" TH.Ppr.<+> TH.Ppr.ppr e TH.Ppr.<+> TH.Ppr.text "}"
pprURule (URuleM ss e) =
  TH.Ppr.hsep (map pprUSym ss) TH.Ppr.<+>
  TH.Ppr.text "{%" TH.Ppr.<+> TH.Ppr.ppr e TH.Ppr.<+> TH.Ppr.text "}"

pprGrammar :: Grammar tok m prods -> TH.Ppr.Doc
pprGrammar = pprUGrammar . toUGrammar
