{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}

module Control.Monad.Reflection.Internal where

import Control.Delimited
import Data.Kind (Type)

-- | Filinski's T monad
-- 
type T :: Type -> (Type -> Type) -> Type -> Type
type family T e m

newtype Eff a = Eff {unEff :: IO a}
    deriving (Functor, Applicative, Monad) via IO


newtype Embed e = Embed (forall a. IO (T e Eff a) -> T e Eff a)

data Handle e r = Handle
    { tag :: PromptTag (T e Eff r),
      embed :: Embed e
    }

-- | Filinski's reify operator
eta
    :: forall e a
     . (Monad (T e Eff))
    => Embed e
    -> (Handle e a -> Eff a)
    -> Eff (T e Eff a)
eta emb act = Eff $ do
    t <- newTag
    reset t (unEff (act (Handle t emb)) >>= \a -> pure (pure a))

-- | Filinski's reflect operator
mu :: forall e a b. (Monad (T e Eff)) => Handle e b -> T e Eff a -> Eff a
mu (Handle t (Embed e)) m = Eff (shift t $ \k -> pure (m >>= e . k))

doIO :: IO a -> Eff a
doIO = Eff

runEff :: Eff a -> IO a
runEff (Eff m) = m
