module Main where

import Test.Hspec
import Test.RetrySpec (spec)

main :: IO ()
main = hspec spec
