{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}

module Control.Monad.Reflection.Internal where

import Control.Delimited
import Control.Delimited.Internal (PromptTag)
import Data.Kind (Type)

-- Eff machinery
type T :: Type -> (Type -> Type) -> Type -> Type
type family T e m

newtype Eff a = Eff {unEff :: IO a}
    deriving (Functor, Applicative, Monad) via IO
    
data Handle e r = Handle
    { tag :: PromptTag (T e Eff r),
      embed :: forall a. IO (T e Eff a) -> T e Eff a
    }

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
reflect (Handle t embed) m = Eff (shift t $ \k -> pure (m >>= embed . k))

io2eff :: IO a -> Eff a
io2eff = Eff

runEff :: Eff a -> IO a
runEff (Eff m) = m
