# reflect-cc

Monadic Reflection as effects.

## How to use

```hs
prog :: Handle (Except String) s -> Handle (State s) s -> Handle (Reader env) r -> Eff s
prog he hs hr = do
    env <- ask hr
    if null env
        then throw he "empty env!"
        else do
            x <- get hs
            put hs (x + 1)
            y <- get hs
            pure y

main :: IO ()
main = runEff (runExcept $ \he -> runState (10 :: Int) $ \hs -> runReader ["not empty"] $ \hr -> prog he hs hr) >>= print
```
