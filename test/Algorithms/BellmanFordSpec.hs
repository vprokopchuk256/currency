module Algorithms.BellmanFordSpec (main, spec) where

import Test.Hspec

import Graph.Graph
import Graph.Edge
import Algorithms.BellmanFord
import Str

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "execute" $ do
    context "with one edge" $ do
      let ab = edge "A" "B" 2.0
      let graph = empty <<< ab
      let tree = execute "A" graph

      it "return tree" $ do
        show tree `shouldBe` join ["A" <-- 0.0, "B" <-- weight ab <-- "A"]

    context "with two edges rooted in the same vertice" $ do
      let ab = edge "A" "B" 2.0
      let ac = edge "A" "C" 2.0
      let graph = empty <<< ab <<< ac
      let tree = execute "A" graph

      it "return tree" $ do
        show tree `shouldBe` join ["A" <-- 0.0,
                                   "B" <-- weight ab <-- "A",
                                   "C" <-- weight ac <-- "A"]

    context "with two sequent edges" $ do
      let ab = edge "A" "B" 2.0
      let bc = edge "B" "C" 2.0
      let graph = empty <<< ab <<< bc
      let tree = execute "A" graph

      it "return tree" $ do
        show tree `shouldBe` join ["A" <-- 0.0,
                                   "B" <-- weight ab <-- "A",
                                   "C" <-- (weight ab + weight bc) <-- "B"]

    -- context "with simple negative cycle" $ do
    --   let ab = edge "A" "B" 2.0
    --   let ba = edge "B" "A" 2.0
    --   let graph = empty <<< ab <<< ba
    --   let tree = execute "A" graph

    --   it "returns detected cycle" $ do
    --     show tree `shouldBe` join ["B" <-- "A" <-- "B"]

    -- context "with simple non negative cycle" $ do
    --   let ab = edge "A" "B" 2.0
    --   let ba = edge "B" "A" 0.5
    --   let graph = empty <<< ab <<< ba
    --   let tree = execute "A" graph

    --   it "returns detected cycle" $ do
    --     show tree `shouldBe` join ["A" <-- 0.0, "B" <-- weight ba <-- "A"]

    -- context "integration example" $ do
    --   let graph= addEdge "USD" "USD" 1.000 .
    --              addEdge "USD" "EUR" 0.741 .
    --              addEdge "USD" "GBP" 0.657 .
    --              addEdge "USD" "CHF" 1.061 .
    --              addEdge "USD" "CAD" 1.011 .
    --              addEdge "EUR" "USD" 1.350 .
    --              addEdge "EUR" "EUR" 1.000 .
    --              addEdge "EUR" "GBP" 0.888 .
    --              addEdge "EUR" "CHF" 1.433 .
    --              addEdge "EUR" "CAD" 1.366 .
    --              addEdge "GBP" "USD" 1.521 .
    --              addEdge "GBP" "EUR" 1.126 .
    --              addEdge "GBP" "GBP" 1.000 .
    --              addEdge "GBP" "CHF" 1.614 .
    --              addEdge "GBP" "CAD" 1.538 .
    --              addEdge "CHF" "USD" 1.943 .
    --              addEdge "CHF" "EUR" 0.698 .
    --              addEdge "CHF" "GBP" 0.620 .
    --              addEdge "CHF" "CHF" 1.000 .
    --              addEdge "CHF" "CAD" 0.953 .
    --              addEdge "CAD" "USD" 0.995 .
    --              addEdge "CAD" "EUR" 0.732 .
    --              addEdge "CAD" "GBP" 0.650 .
    --              addEdge "CAD" "CHF" 1.049 .
    --              addEdge "CAD" "CAD" 1.000 $ empty
    --   let tree = execute "USD" graph

    --   it "return tree" $ do
    --     show tree `shouldBe` join ["A" <-- 0.0]
