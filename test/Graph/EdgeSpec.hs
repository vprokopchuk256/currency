module Graph.EdgeSpec (main, spec) where

import Test.Hspec

import qualified Graph.Edge as Edge

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "edge" $ do
    let edge = Edge.edge "USD" "CHF" 10.0

    it "sets edge props and weight" $ do
      edge `shouldBe` (Edge.Edge "USD" "CHF" 10.0 (-log 10.0))

  describe "show" $ do
    let strEdge = show $ Edge.edge "USD" "CHF" 10.0

    it "generates proper string representation" $ do
      strEdge `shouldBe` "USD <- 10.0 <- CHF"
