module Example where

import Euphoric
import Control.Monad.Trans.State

data ExToken =
  ExTokTrue |
  ExTokFalse |
  ExTokLPar |
  ExTokRPar

class ExResult r where
  true :: r
  false :: r
  par :: r -> r

newtype ExValue = EV (forall r. ExResult r => r)

grammar :: Grammar ExToken (State Int) _
grammar = MkGrammar
  $ tm "true"  [p|ExTokTrue|]
  $ tm "false" [p|ExTokFalse|]
  $ tm "("     [p|ExTokLPar|]
  $ tm ")"     [p|ExTokRPar|]
  $ nt "lit" ExValue
    do ["true"]  ==> [|| \_ -> EV true ||]
       ["false"] ==> [|| \_ -> EV false ||]
  $ nt "expr" ExValue
    do ["(", "expr", ")"] ==>% [|| \_ (EV e) _ -> EV (par e) <$ modify (+1) ||]
       ["lit"]            ==>  [|| id ||]
  $ EndOfGrammar
