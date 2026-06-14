{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Reflection.Cont where

import Control.Monad.Reflection
import Control.Monad.Trans.Cont (ContT (ContT, runContT))
import Data.Kind (Type)

data Cont (r :: Type)

type instance T (Cont r) Eff = ContT r Eff

shiftC :: SomeHandle (Cont r) -> ((a -> Eff r) -> Eff r) -> Eff a
shiftC h f = reflect h (ContT f)

runCont :: (a -> Eff r) -> Scope (Cont r) a -> Eff r
runCont c scope = reify contEmbed scope >>= \r -> runContT r c
  where
    contEmbed = Embed (\io -> ContT $ \cc -> doIO io >>= \t -> runContT t cc)
