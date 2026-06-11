{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Reflection.Internal (module Control.Monad.Reflection.Internal) where

import Control.Delimited.Internal (PromptTag)
import Data.Kind (Type)

-- Eff machinery
type T :: Type -> (Type -> Type) -> Type -> Type

type family T e m

newtype Eff a = Eff { unEff :: IO a }

instance Functor Eff where
    fmap f (Eff m) = Eff (fmap f m)

instance Applicative Eff where
    pure a = Eff (pure a)
    (Eff f) <*> (Eff a) = Eff (f <*> a)

instance Monad Eff where
    (Eff m) >>= f = Eff (m >>= unEff . f)

data Handle e r = Handle 
    { tag :: PromptTag (T e Eff r)
    , embed :: forall a. IO (T e Eff a) -> T e Eff a
    }