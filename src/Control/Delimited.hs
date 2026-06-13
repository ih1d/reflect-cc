{-# LANGUAGE TypeFamilies #-}

module Control.Delimited
    ( PromptTag,
      reset,
      shift0,
      control,
      shift,
      newTag,
    ) where

import Control.Delimited.Internal

-- IO machinery
reset :: PromptTag a -> IO a -> IO a
reset = prompt

shift0 :: PromptTag a -> ((b -> IO a) -> IO a) -> IO b
shift0 t f = control0 t $ \k -> f (reset t . k . pure)

control :: PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
control t f = control0 t $ \k -> reset t (f k)

shift :: PromptTag a -> ((b -> IO a) -> IO a) -> IO b
shift t f = control0 t $ \k -> reset t (f (reset t . k . pure))

newTag :: IO (PromptTag a)
newTag = newPromptTag
