module Euphoric where

import Data.Kind (Type)
import GHC.TypeLits (Symbol, KnownSymbol, TypeError, ErrorMessage(..))
import Control.Monad.Trans.State
import Language.Haskell.TH (PatQ, TExpQ)

data Grammar tok m prods = MkGrammar (GrammarBuilder tok m prods prods)

data GrammarBuilder tok m (allProds :: [(Symbol, Type)]) (prods :: [(Symbol, Type)]) where
  EndOfGrammar :: GrammarBuilder tok m allProds '[]
  GrammarCons ::
    Prod tok m allProds ty ->
    GrammarBuilder tok m allProds prods ->
    GrammarBuilder tok m allProds ('(sym, ty) : prods)

data Symbols xs where
  SymbolsNil :: Symbols '[]
  SymbolsCons :: KnownSymbol x => Symbols xs -> Symbols (x : xs)

class Syms xs where
  syms :: Symbols xs
instance Syms '[] where
  syms = SymbolsNil
instance (KnownSymbol x, Syms xs) => Syms (x : xs) where
  syms = SymbolsCons syms

type family Repr (prods :: [(Symbol, Type)]) (sym :: Symbol) where
  Repr '[] sym = TypeError ('Text sym ':<>: 'Text " is not a symbol of the grammar")
  Repr ('(sym, ty):_) sym = ty
  Repr (_:prods) sym = Repr prods sym

type family WithReprs allProds syms ty where
  WithReprs allProds '[] ty = ty
  WithReprs allProds (sym:syms) ty =
    Repr allProds sym -> WithReprs allProds syms ty

data Rule allProds m ty =
  forall syms. MkRule  (Symbols syms) (TExpQ (WithReprs allProds syms ty)) |
  forall syms. MkRuleM (Symbols syms) (TExpQ (WithReprs allProds syms (m ty)))

data Prod tok m (allProds :: [(Symbol, Type)]) ty =
  MkTerm PatQ |
  MkNonTerm [Rule allProds m ty]

rule ::
  forall syms allProds m ty.
  Syms syms =>
  TExpQ (WithReprs allProds syms ty) ->
  State [Rule allProds m ty] ()
rule reduction = modify (MkRule (syms @syms) reduction:)

ruleM ::
  forall syms allProds m ty.
  Syms syms =>
  TExpQ (WithReprs allProds syms (m ty)) ->
  State [Rule allProds m ty] ()
ruleM reduction = modify (MkRuleM (syms @syms) reduction:)

tm ::
  forall sym tok m allProds prods.
  PatQ ->
  GrammarBuilder tok m allProds prods ->
  GrammarBuilder tok m allProds ('(sym, tok) : prods)
tm f = GrammarCons (MkTerm f)

nt ::
  forall sym ty tok m allProds prods.
  State [Rule allProds m ty] () ->
  GrammarBuilder tok m allProds prods ->
  GrammarBuilder tok m allProds ('(sym, ty) : prods)
nt s = GrammarCons (MkNonTerm (execState s []))

-----------------------------------------------------------

data ExToken =
  ExTokTrue |
  ExTokFalse |
  ExTokLPar |
  ExTokRPar

data ExRepr = ExPar ExRepr | ExBLit Bool

grammar :: Grammar ExToken (State Int) _
grammar = MkGrammar
  $ tm @"true"  [p|ExTokTrue|]
  $ tm @"false" [p|ExTokFalse|]
  $ tm @"(" [p|ExTokLPar|]
  $ tm @")" [p|ExTokRPar|]
  $ nt @"expr" @ExRepr
    do ruleM @'["(", "expr", ")"]  [|| \_ e _ -> ExPar e <$ modify (+1) ||]
       rule  @'["true"]            [|| \_ -> ExBLit True ||]
       rule  @'["false"]           [|| \_ -> ExBLit False ||]
  $ EndOfGrammar
