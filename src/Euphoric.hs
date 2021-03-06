module Euphoric where

import Data.Kind (Type)
import Data.Proxy
import GHC.TypeLits
import Control.Monad.Trans.State
import Language.Haskell.TH (Pat, TExp, Exp)
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Ppr as TH.Ppr
import qualified Language.Haskell.TH.PprLib as TH.Ppr
import MiniQ

data Grammar tok m prods = MkGrammar (GrammarBuilder tok m prods prods)

data GrammarBuilder tok m (allProds :: [(Symbol, Type)]) (prods :: [(Symbol, Type)]) where
  EndOfGrammar :: GrammarBuilder tok m allProds '[]
  GrammarCons ::
    KnownSymbol sym =>
    Proxy sym ->
    Prod tok m allProds ty ->
    GrammarBuilder tok m allProds prods ->
    GrammarBuilder tok m allProds ('(sym, ty) : prods)

data Symbols xs where
  SymbolsNil :: Symbols '[]
  SymbolsCons :: KnownSymbol x => Proxy x -> Symbols xs -> Symbols (x : xs)

class Syms xs where
  syms :: Symbols xs
instance Syms '[] where
  syms = SymbolsNil
instance (KnownSymbol x, Syms xs) => Syms (x : xs) where
  syms = SymbolsCons Proxy syms

symbolsVal :: Symbols xs -> [String]
symbolsVal SymbolsNil = []
symbolsVal (SymbolsCons symProxy symbols) = symbolVal symProxy : symbolsVal symbols

type family Repr (prods :: [(Symbol, Type)]) (sym :: Symbol) where
  Repr '[] sym = TypeError ('Text sym ':<>: 'Text " is not a symbol of the grammar")
  Repr ('(sym, ty):_) sym = ty
  Repr (_:prods) sym = Repr prods sym

type family WithReprs allProds syms ty where
  WithReprs allProds '[] ty = ty
  WithReprs allProds (sym:syms) ty =
    Repr allProds sym -> WithReprs allProds syms ty

data Rule allProds m ty =
  forall syms. MkRule  (Symbols syms) (TExp (WithReprs allProds syms ty)) |
  forall syms. MkRuleM (Symbols syms) (TExp (WithReprs allProds syms (m ty)))

data Prod tok m (allProds :: [(Symbol, Type)]) ty =
  MkTerm Pat |
  MkNonTerm [Rule allProds m ty]

rule ::
  forall syms allProds m ty.
  Syms syms =>
  TH.Code MiniQ (WithReprs allProds syms ty) ->
  State [Rule allProds m ty] ()
rule reduction = modify (MkRule (syms @syms) (runMiniQ (TH.examineCode reduction)):)

ruleM ::
  forall syms allProds m ty.
  Syms syms =>
  TH.Code MiniQ (WithReprs allProds syms (m ty)) ->
  State [Rule allProds m ty] ()
ruleM reduction = modify (MkRuleM (syms @syms) (runMiniQ (TH.examineCode reduction)):)

tm ::
  forall sym tok m allProds prods.
  KnownSymbol sym =>
  MiniQ Pat ->
  GrammarBuilder tok m allProds prods ->
  GrammarBuilder tok m allProds ('(sym, tok) : prods)
tm f = GrammarCons Proxy (MkTerm (runMiniQ f))

nt ::
  forall sym ty tok m allProds prods.
  KnownSymbol sym =>
  State [Rule allProds m ty] () ->
  GrammarBuilder tok m allProds prods ->
  GrammarBuilder tok m allProds ('(sym, ty) : prods)
nt s = GrammarCons Proxy (MkNonTerm (execState s []))

data UGrammar = UGrammar [(String, Pat)] [(String, [URule])]

data URule = URule [String] Exp | URuleM [String] Exp

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
toURule (MkRule symbols texp) =
  URule (symbolsVal symbols) (TH.unType texp)
toURule (MkRuleM symbols texp) =
  URuleM (symbolsVal symbols) (TH.unType texp)

pprUSym :: String -> TH.Ppr.Doc
pprUSym = TH.Ppr.quotes . TH.Ppr.text

pprUGrammar :: UGrammar -> TH.Ppr.Doc
pprUGrammar (UGrammar utms unts) =
  TH.Ppr.hang (TH.Ppr.text "%token") 2
    (TH.Ppr.vcat (map pprUTerm utms))
  TH.Ppr.$$
  TH.Ppr.text "%%" TH.Ppr.$$
    TH.Ppr.vcat (map pprUNonTerm unts)

pprUTerm :: (String, Pat) -> TH.Ppr.Doc
pprUTerm (s, pat) = pprUSym s TH.Ppr.<+> TH.Ppr.braces (TH.Ppr.ppr pat)

pprUNonTerm :: (String, [URule]) -> TH.Ppr.Doc
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
