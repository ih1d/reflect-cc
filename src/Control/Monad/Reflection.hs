module Control.Monad.Reflection (
    Eff,
    Embed(..),
    Handle,
    Scope(..),
    SomeHandle(..),
    T,
    reify,
    reflect,
    doIO,
    runEff,
) where

import Control.Monad.Reflection.Internal

newtype Scope e a = Scope (forall r. (Handle e r -> Eff a))

data SomeHandle e = forall r. SomeHandle (Handle e r)

reify :: (Monad (T e Eff)) => Embed e -> Scope e a -> Eff (T e Eff a)
reify emb (Scope act) = eta emb act

reflect :: (Monad (T e Eff)) => SomeHandle e -> T e Eff a -> Eff a
reflect (SomeHandle h) t = mu h t