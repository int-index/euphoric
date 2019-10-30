module MiniQ (runMiniQ) where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Fail
import Language.Haskell.TH (Q, runQ)
import Language.Haskell.TH.Syntax (Quasi(..), mkNameU)

newtype MiniQ a = MkMiniQ (State Int a)
  deriving newtype (Functor, Applicative, Monad)

runMiniQ :: Q a -> a
runMiniQ m = case runQ m of MkMiniQ m' -> evalState m' 0

instance MonadFail MiniQ where
  fail = error

instance MonadIO MiniQ where
  liftIO = error "MiniQ: liftIO"

instance Quasi MiniQ where
  qNewName s = MkMiniQ $ state $ \i -> (mkNameU s i, i + 1)
  qReport = error "MiniQ: qReport"
  qRecover = error "MiniQ: qRecover"
  qLookupName = error "MiniQ: qLookupName"
  qReify = error "MiniQ: qReify"
  qReifyFixity = error "MiniQ: qReifyFixity"
  qReifyInstances = error "MiniQ: qReifyInstances"
  qReifyRoles = error "MiniQ: qReifyRoles"
  qReifyAnnotations = error "MiniQ: qReifyAnnotations"
  qReifyModule = error "MiniQ: qReifyModule"
  qReifyConStrictness = error "MiniQ: qReifyConStrictness"
  qLocation = error "MiniQ: qLocation"
  qAddDependentFile = error "MiniQ: qAddDependentFile"
  qAddTempFile = error "MiniQ: qAddTempFile"
  qAddTopDecls = error "MiniQ: qAddTopDecls"
  qAddForeignFilePath = error "MiniQ: qAddForeignFilePath"
  qAddModFinalizer = error "MiniQ: qAddModFinalizer"
  qAddCorePlugin = error "MiniQ: qAddCorePlugin"
  qGetQ = error "MiniQ: qGetQ"
  qPutQ = error "MiniQ: qPutQ"
  qIsExtEnabled = error "MiniQ: qIsExtEnabled"
  qExtsEnabled = error "MiniQ: qExtsEnabled"

