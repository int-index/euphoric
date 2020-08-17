module Example where

import Euphoric
import Control.Monad.Trans.State

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
  $ nt @"lit" @ExRepr
    do rule  @'["true"]            [|| \_ -> ExBLit True ||]
       rule  @'["false"]           [|| \_ -> ExBLit False ||]
  $ nt @"expr" @ExRepr
    do ruleM @'["(", "expr", ")"]  [|| \_ e _ -> ExPar e <$ modify (+1) ||]
       rule  @'["lit"]             [|| id ||]
  $ EndOfGrammar
