module Test.Wyag (wyagSpec) where

import Test.Hspec (Spec, describe)
import Test.Wyag.Core.Object (objectSpec)

wyagSpec :: Spec
wyagSpec = describe "Wyag" $ do
  objectSpec