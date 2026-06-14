{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Reflection.Except where

import Control.Monad.Reflection
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Kind (Type)

data Except (s :: Type)

type instance T (Except e) Eff = ExceptT e Eff

throw :: SomeHandle (Except e) -> e -> Eff a
throw h err = reflect h (ExceptT (pure (Left err)))

runExcept :: Scope (Except e) a -> Eff (Either e a)
runExcept scope = reify exceptEmbed scope >>= runExceptT
  where
    exceptEmbed = Embed (\io -> ExceptT (doIO io >>= \r -> runExceptT r))
