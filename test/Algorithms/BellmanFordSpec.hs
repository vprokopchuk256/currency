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
      let relaxable = execute "A" graph

      it "return tree" $ do
        show relaxable `shouldBe` join ["A" <-- 0.0, "B" <-- weight ab <-- "A"]

    context "with two edges rooted in the same vertice" $ do
      let ab = edge "A" "B" 2.0
      let ac = edge "A" "C" 2.0
      let graph = empty <<< ab <<< ac
      let relaxable = execute "A" graph

      it "return tree" $ do
        show relaxable `shouldBe` join ["A" <-- 0.0,
                                        "B" <-- weight ab <-- "A",
                                        "C" <-- weight ac <-- "A"]

    context "with two sequent edges" $ do
      let ab = edge "A" "B" 2.0
      let bc = edge "B" "C" 2.0
      let graph = empty <<< ab <<< bc
      let relaxable = execute "A" graph

      it "return tree" $ do
        show relaxable `shouldBe` join ["A" <-- 0.0,
                                        "B" <-- weight ab <-- "A",
                                        "C" <-- (weight ab + weight bc) <-- "B"]

    context "with simple negative cycle" $ do
      let ab = edge "A" "B" 0.741
      let ba = edge "B" "A" 1.350
      let graph = empty <<< ab <<< ba
      let relaxable = execute "A" graph

      it "returns detected cycle" $ do
        show relaxable `shouldBe` "B" <-- "A" <-- "B"

    context "with simple non negative cycle" $ do
      let ab = edge "A" "B" 0.741
      let ba = edge "B" "A" 1.250
      let graph = empty <<< ab <<< ba
      let relaxable = execute "A" graph

      it "returns shortest paths tree" $ do
        show relaxable `shouldBe` join ["A" <-- 0.0, "B" <-- weight ab <-- "A"]

    context "with special cycle" $ do
      let graph= addEdge "A" "B" 0.741 .
                 addEdge "A" "C" 0.657 .
                 addEdge "A" "D" 1.061 .
                 addEdge "A" "E" 1.011 .
                 addEdge "B" "C" 0.888 .
                 addEdge "C" "A" 1.521 .
                 addEdge "C" "D" 1.614 .
                 addEdge "C" "E" 1.538 .
                 addEdge "D" "A" 1.943 .
                 addEdge "D" "B" 0.698 $ empty
      let relaxable = execute "A" graph

      it "returns cycle" $ do
        show relaxable `shouldBe` "C" <-- "A" <-- "B" <-- "C"
