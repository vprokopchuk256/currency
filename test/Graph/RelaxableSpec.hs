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
    describe "relax" $ do
        context "one edge without cycles" $ do
            let graph = addEdge "A" "B" 10.0 . addEdge "C" "B" 5.0 $ empty
            let tree = start "A" graph

            it "returns tree with relaxed edge is provided first time" $ do
              let ab = edge "A" "B" 3.0
              let relaxedTree = relax ab tree

              show relaxedTree `shouldBe` join ["A" <-- 0.0,
                                                "B" <-- weight ab <-- "A",
                                                "C" <-- "Nothing"]

            context "and when one more shorter edge is specified" $ do
              let cb = edge "C" "B" 5.0
              let ac = edge "A" "C" 3.0
              let ab = edge "A" "B" 3.0
              let relaxedTree = relax cb . relax ac . relax ab $ tree

              it "returns tree with edge relaxed one more time" $ do
                show relaxedTree `shouldBe` join ["A" <-- 0.0,
                                                  "B" <-- (weight ac + weight cb)  <-- "C",
                                                  "C" <-- weight ac <-- "A" ]

            context "and when longer edge is specified" $ do
              let ab0 = edge "A" "B" 3.0
              let ab1 = edge "A" "B" 2.0
              let relaxedTree = relax ab1 . relax ab0 $ tree

              it "returns non modified tree" $ do
                show relaxedTree `shouldBe` join ["A" <-- 0.0,
                                                  "B" <-- weight ab0 <-- "A",
                                                  "C" <-- "Nothing"]
