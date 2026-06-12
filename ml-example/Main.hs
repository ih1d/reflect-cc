module Main where

import Control.Monad.Reflection
import Control.Monad.Reflection.Except
import Control.Monad.Reflection.Reader
import Control.Monad.Reflection.State
import Data.IORef
import Control.Monad ((>=>))

newtype Cell a = Cell (IORef a)

prog hr he hs = do
    env <- ask hr
    if env /= "" then get hs >>= \a -> pure (a + 2) else throw he "error"
    
main :: IO ()
main = do
    mint <- runEff $ runReader "" $ \hr -> runExcept $ \he -> runState (1 :: Int) $ \hs -> prog hr he hs
    case mint of
        Left err -> putStrLn err
        Right int -> print int

main2 :: IO ()
main2 = do
    let er = runReader "string" ask
        ee = runExcept (`throw` "reverse")
        es = runState (1 :: Int) (get >=> (pure . (+ 2)))
    rr <- runEff er
    _ere <- runEff ee
    rs <- runEff es
    print (rr, rs) -- cannot print _ere due to being ambiguous