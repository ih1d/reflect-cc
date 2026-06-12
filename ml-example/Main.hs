module Main where

import Control.Monad.Reflection.State (State)
import Control.Monad.Reflection

data Value a = Value
    { val :: a
    , dval :: a
    , children :: [a]
    , op :: Char
    }

main :: IO ()
main = undefined