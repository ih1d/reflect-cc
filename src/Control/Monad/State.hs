{-# LANGUAGE TypeFamilies #-}
module Control.Monad.State where

import Control.Monad.Reflection.Internal
import Control.Monad.Reflection
import Control.Monad.Trans.State (StateT(StateT, runStateT))
import Data.Kind (Type)

data State (s :: Type)

type instance T (State s) m = StateT s m

get :: Handle (State s) r -> Eff s
get h = reflect h (StateT $ \s -> pure (s, s))

put :: Handle (State s) r -> s -> Eff ()
put h s = reflect h (StateT $ \_ -> pure ((), s))

runState :: s -> (Handle (State s) a -> Eff a) -> Eff (a, s)
runState s0 body = reify (\io -> StateT $ \s -> io2eff io >>= \t -> runStateT t s) body >>= \res -> runStateT res s0