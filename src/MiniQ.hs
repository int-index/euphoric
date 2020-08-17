module MiniQ (MiniQ, runMiniQ) where

import Control.Monad.Trans.State
import Language.Haskell.TH.Syntax (Quote(..), mkNameU)

newtype MiniQ a = MkMiniQ (State Integer a)
  deriving newtype (Functor, Applicative, Monad)

runMiniQ :: MiniQ a -> a
runMiniQ (MkMiniQ m) = evalState m 0

instance Quote MiniQ where
  newName s = MkMiniQ $ state $ \i -> (mkNameU s i, i + 1)
