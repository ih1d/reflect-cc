{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Reflection.Reader where

import Control.Monad.Reflection
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import Data.Kind (Type)

data Reader (r :: Type)

type instance T (Reader r) Eff = ReaderT r Eff

ask :: SomeHandle (Reader r) -> Eff r
ask h = reflect h (ReaderT pure)

runReader :: r -> Scope (Reader r) r -> Eff r
runReader r scope = 
    reify readerEmbed scope >>= \res -> runReaderT res r
    where
        readerEmbed = Embed (\io -> ReaderT (\r' -> doIO io >>= \t -> runReaderT t r'))