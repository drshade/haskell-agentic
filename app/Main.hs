module Main where

import Examples (loudJokeTeller)
import Simple (run)

main :: IO ()
main = do
    result <- run loudJokeTeller ""
    print result
