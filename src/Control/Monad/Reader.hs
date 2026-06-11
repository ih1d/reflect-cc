{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Reader where

import Control.Monad.Reflection
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import Data.Kind (Type)

data Reader (r :: Type)

type instance T (Reader r) m = ReaderT r m

ask :: Handle (Reader r) a -> Eff r
ask h = reflect h (ReaderT $ \r -> pure r)

runReader :: r -> (Handle (Reader r) a -> Eff a) -> Eff a
runReader r body =
    reify (\io -> ReaderT $ \r' -> io2eff io >>= \t -> runReaderT t r') body >>= \res -> runReaderT res r
