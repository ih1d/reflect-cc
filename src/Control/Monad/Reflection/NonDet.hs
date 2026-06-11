{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Reflection.NonDet where

import ListT
import Control.Monad.Reflection
import Control.Applicative (Alternative(empty))
import Control.Monad (join)
import Control.Monad.Trans.Class

data NonDet

type instance T NonDet m = ListT m

fromListT :: Handle NonDet r -> ListT Eff a -> Eff a
fromListT = reflect

choose :: Handle NonDet r -> [a] -> Eff a
choose h = fromListT h . fromFoldable

failure :: Handle NonDet r -> Eff a
failure h = fromListT h empty

runNonDetT :: (Handle NonDet r0 -> Eff r0) -> Eff (T NonDet Eff r0)
runNonDetT = reify (\io -> join (lift (io2eff io)))

runNonDet :: (forall r. Handle NonDet r -> Eff a) -> Eff [a]
runNonDet body = runNonDetT body >>= toList