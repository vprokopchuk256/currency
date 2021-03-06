module Graph.RelaxableSpec (main, spec) where

import Test.Hspec

import Graph.Edge
import Graph.Relaxable
import Graph.Graph
import Str

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    let graph = addEdge "A" "B" 10.0 . addEdge "C" "B" 5.0 $ empty
    let relaxable = tree "A" graph

    describe "relax" $ do
        context "one edge without cycles" $ do
            it "returns tree with relaxed edge is provided first time" $ do
                let ab = edge "A" "B" 3.0
                let relaxedTree = relax relaxable [ab]

                show relaxedTree `shouldBe` join ["A" <-- 0.0,
                                                  "B" <-- weight ab <-- "A",
                                                  "C" <-- "Nothing"]

        context "and when one more shorter edge is specified" $ do
            let cb = edge "C" "B" 5.0
            let ac = edge "A" "C" 3.0
            let ab = edge "A" "B" 3.0
            let relaxedTree = relax relaxable [ab, ac, cb]

            it "returns tree with edge relaxed one more time" $ do
                show relaxedTree `shouldBe` join ["A" <-- 0.0,
                                                  "B" <-- (weight ac + weight cb)  <-- "C",
                                                  "C" <-- weight ac <-- "A" ]

        context "and when longer edge is specified" $ do
            let ab0 = edge "A" "B" 3.0
            let ab1 = edge "A" "B" 2.0
            let relaxedTree = relax relaxable [ab0, ab1]

            it "returns non modified tree" $ do
                show relaxedTree `shouldBe` join ["A" <-- 0.0,
                                                  "B" <-- weight ab0 <-- "A",
                                                  "C" <-- "Nothing"]

        context "without cycles" $ do
          let cb = edge "C" "B" 5.0
          let ac = edge "A" "C" 3.0
          let ab = edge "A" "B" 3.0
          let relaxedTree = relax relaxable [ab, ac, cb]

          it "returns tree with all specified edges relaxed" $ do
              show relaxedTree `shouldBe` join ["A" <-- 0.0,
                                                "B" <-- (weight ac + weight cb)  <-- "C",
                                                "C" <-- weight ac <-- "A" ]

    describe "detectCycle" $ do
        context "without cycles" $ do
          let cb = edge "C" "B" 5.0
          let ac = edge "A" "C" 3.0
          let ab = edge "A" "B" 3.0
          let relaxedTree = relax relaxable [ab, ac, cb]
          let relaxed = detectCycle [ab, ac, cb] relaxedTree

          it "returns tree with all specified edges relaxed" $ do
              relaxed `shouldBe` relaxedTree

        context "and negative cycle detected" $ do
            let ab = edge "A" "B" 3.0
            let bc = edge "B" "C" 3.0
            let ca = edge "C" "A" 3.0
            let relaxedTree = relax relaxable [ab, bc, ca]

            let relaxed = detectCycle [ab, bc, ca] relaxedTree

            it "returns detected cycle" $ do
              relaxed `shouldBe` Cycle ["C", "A", "B", "C"]
