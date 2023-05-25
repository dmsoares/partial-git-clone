module Test.Wyag.Core.Object where

import Data.Byteable
import Test.Hspec (Spec, describe, it, shouldBe)
import Wyag.Core.Object
import Wyag.Core.Parser
import Wyag.Core.Tree

tree :: GitObject
tree = GitTree (Tree [TreeEntry "1234" "bar" "1234"])

blob :: GitObject
blob = GitBlob (Blob "yada yada")

objectSpec :: Spec
objectSpec = describe "Object - deserialize / serialize" do
  it "should be isomorphisms" do
    let Right expectedBlob = parse objectP "object" (toBytes blob)
    expectedBlob `shouldBe` blob

    let Right expectedTree = parse objectP "object" (toBytes tree)
    expectedTree `shouldBe` tree