module Euphoric where

import Data.Kind (Type)
import GHC.TypeLits (Symbol, KnownSymbol, TypeError, ErrorMessage(..))
import Control.Monad.Trans.State

data Grammar tok prods = MkGrammar (GrammarBuilder tok prods prods)

data GrammarBuilder tok (allProds :: [(Symbol, Type)]) (prods :: [(Symbol, Type)]) where
  EndOfGrammar :: GrammarBuilder tok allProds '[]
  GrammarCons ::
    Prod tok allProds ty ->
    GrammarBuilder tok allProds prods ->
    GrammarBuilder tok allProds ('(sym, ty) : prods)

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

data Rule allProds ty =
  forall syms. MkRule (Symbols syms) (WithReprs allProds syms ty)

data Prod tok (allProds :: [(Symbol, Type)]) ty =
  MkTerm (tok -> Maybe ty) |
  MkNonTerm [Rule allProds ty]

rule ::
  forall syms allProds ty.
  Syms syms =>
  WithReprs allProds syms ty ->
  State [Rule allProds ty] ()
rule reduction = modify (MkRule (syms @syms) reduction:)

tm ::
  forall sym ty tok allProds prods.
  (tok -> Maybe ty) ->
  GrammarBuilder tok allProds prods ->
  GrammarBuilder tok allProds ('(sym, ty) : prods)
tm f = GrammarCons (MkTerm f)

nt ::
  forall sym ty tok allProds prods.
  State [Rule allProds ty] () ->
  GrammarBuilder tok allProds prods ->
  GrammarBuilder tok allProds ('(sym, ty) : prods)
nt s = GrammarCons (MkNonTerm (execState s []))

-----------------------------------------------------------

data ExToken =
  ExTokTrue |
  ExTokFalse |
  ExTokLPar |
  ExTokRPar

data ExRepr = ExPar ExRepr | ExBLit Bool

grammar :: Grammar ExToken _
grammar = MkGrammar
  $ tm @"true" @Bool
    \case ExTokTrue -> Just True
          _ -> Nothing
  $ tm @"false" @Bool
    \case ExTokFalse -> Just False
          _ -> Nothing
  $ tm @"(" @()
    \case ExTokLPar -> Just ()
          _ -> Nothing
  $ tm @")" @()
    \case ExTokRPar -> Just ()
          _ -> Nothing
  $ nt @"expr" @ExRepr
    do rule @'["(", "expr", ")"]     \_ e _ -> ExPar e
       rule @'["true"]               \t -> ExBLit t
       rule @'["false"]              \t -> ExBLit t
  $ EndOfGrammar
