module Main (main) where

import Test.Hspec (hspec)
import Test.Wyag (wyagSpec)

main :: IO ()
main = hspec wyagSpec