module Graph.EdgeSpec (main, spec) where

import Test.Hspec

import qualified Graph.Edge as Edge

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "new" $ do
    let edge = Edge.new "USD" "CHF" 10.0

    it "sets from" $ do
      (Edge.from edge) `shouldBe` "USD"

    it "sets to" $ do
      (Edge.to edge) `shouldBe` "CHF"

    it "sets to" $ do
      (Edge.rate edge) `shouldBe` 10.0

    it "sets weight" $ do
      (Edge.weight edge) `shouldBe` (-log 10.0)
