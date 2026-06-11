{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Except where

import Control.Monad.Reflection.Internal
import Control.Monad.Reflection
import Data.Kind (Type)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)

data Except (s :: Type)

type instance T (Except e) m = ExceptT e m

throw :: Handle (Except e) r -> e -> Eff a
throw h err = reflect h (ExceptT (pure (Left err)))

runError :: (Handle (Except e) a -> Eff a) -> Eff (Either e a)
runError body = reify (\io -> ExceptT (io2eff io >>= \r -> runExceptT r)) body >>= runExceptT