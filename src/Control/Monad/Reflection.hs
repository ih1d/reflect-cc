module Control.Monad.Reflection where

import Control.Delimited
import Control.Monad.Reflection.Internal

reify
    :: forall e a
     . (Monad (T e Eff))
    => (forall r. IO (T e Eff r) -> T e Eff r)
    -> (Handle e a -> Eff a)
    -> Eff (T e Eff a)
reify emb act = Eff $ do
    t <- newTag
    reset t (unEff (act (Handle t emb)) >>= \a -> pure (pure a))

reflect :: (Monad (T e Eff)) => Handle e b -> T e Eff a -> Eff a
reflect (Handle t embed) m = Eff (shift t $ \k -> pure (m >>= \a -> embed (k a)))

io2eff :: IO a -> Eff a
io2eff = Eff

runEff :: Eff a -> IO a
runEff (Eff m) = m
