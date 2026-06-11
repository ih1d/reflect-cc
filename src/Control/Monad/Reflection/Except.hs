{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Reflection.Except where

import Control.Monad.Reflection
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Kind (Type)

data Except (s :: Type)

type instance T (Except e) m = ExceptT e m

throw :: Handle (Except e) r -> e -> Eff a
throw h err = reflect h (ExceptT (pure (Left err)))

runExcept :: (Handle (Except e) a -> Eff a) -> Eff (Either e a)
runExcept body = reify (\io -> ExceptT (io2eff io >>= \r -> runExceptT r)) body >>= runExceptT
