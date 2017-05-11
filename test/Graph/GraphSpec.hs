module Graph.GraphSpec (main, spec) where

import Test.Hspec

import qualified Data.List as List

import Graph.Edge
import qualified Graph.Graph as Graph

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "empty graph" $ do
    let graph = Graph.empty :: Graph.Graph String

    it "has zero size" $ do
      (Graph.size graph) `shouldBe` 0

    it "has empty vertices list" $ do
      (Graph.vertices graph) `shouldBe` []

    it "has empty edges list" $ do
      (Graph.edges graph) `shouldBe` []

    it "has empty description" $ do
      (show graph) `shouldBe` ""

  describe "graph with one edge" $ do
    let graph = Graph.addEdge "USD" "CHF" 10.0 Graph.empty

    it "has size equals 2" $ do
      (Graph.size graph) `shouldBe` 2

    it "has vertices list that includes both sides of the edge" $ do
      (List.sort (Graph.vertices graph)) `shouldBe` ["CHF", "USD"]

    it "has edges list with one just inserted edge" $ do
      (Graph.edges graph) `shouldBe` [edge "USD" "CHF" 10.0]

  describe "with two connected edges" $ do
    let graph = (Graph.addEdge "USD" "CHF" 10.0) .
                (Graph.addEdge "CHF" "EUR" 20.0) $ Graph.empty

    it "has size equals 3" $ do
      (Graph.size graph) `shouldBe` 3

    it "has vertices list that includes all sides of the edges" $ do
      (List.sort (Graph.vertices graph)) `shouldBe` ["CHF", "EUR", "USD"]

    it "has edges list with all inserted edges" $ do
      (Graph.edges graph) `shouldBe` [(edge "CHF" "EUR" 20.0), (edge "USD" "CHF" 10.0)]
