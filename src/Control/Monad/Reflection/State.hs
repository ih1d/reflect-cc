{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Reflection.State where

import Control.Monad.Reflection
import Control.Monad.Trans.State (StateT (StateT, runStateT))
import Data.Kind (Type)

data State (s :: Type)

type instance T (State s) Eff = StateT s Eff

get :: SomeHandle (State s) -> Eff s
get h = reflect h (StateT $ \s -> pure (s, s))

put :: SomeHandle (State s) -> s -> Eff ()
put h s = reflect h (StateT $ \_ -> pure ((), s))

runState :: s -> Scope (State s) a -> Eff (a, s)
runState s0 scope = reify stateEmbed scope >>= \res -> runStateT res s0
  where
    stateEmbed = Embed (\io -> StateT $ \s -> doIO io >>= \t -> runStateT t s)
