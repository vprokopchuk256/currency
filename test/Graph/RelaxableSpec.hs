module Graph.RelaxableSpec (main, spec) where

import Test.Hspec

import qualified Data.HashMap.Strict as Map
import qualified Data.List as List

import qualified Graph.Edge as Edge
import qualified Graph.Graph as Graph
import qualified Graph.Relaxable as Relaxable
import Str

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "show" $ do
        describe "tree" $ do
            context "when empty" $ do
                let tree = show $ (Relaxable.Tree Map.empty :: Relaxable.Relaxable String)

                it "it returns empty string" $ do
                    tree `shouldBe` ""

            context "when has nodes without previous info and distance" $ do
                let tree = show $ Relaxable.Tree (Map.fromList [("A", Nothing)])

                it "it returns string with node information" $ do
                    tree `shouldBe` "A" <-- "Nothing"

            context "when has nodes with previous not and distance info" $ do
                let tree = show $ Relaxable.Tree (Map.fromList [("A", (Just (123.45, Just "B")))])

                it "it returns string with routing information" $ do
                    tree `shouldBe` "A <- 123.45 <- B"

        describe "cycle" $ do
            context "when empty" $ do
                let cycle = show $ (Relaxable.Cycle [] 0.0 :: Relaxable.Relaxable String)

                it "it returns empty string" $ do
                    cycle `shouldBe` ""

            context "when has nodes" $ do
                let cycle = (Relaxable.Cycle ["A", "B"] 0.45 :: Relaxable.Relaxable String)

                it "it returns string with routing information" $ do
                    show cycle `shouldBe` "A <- B (0.45)"

    describe "fromGraph" $ do
        let graph = Graph.addEdge "A" "B" 10.0 Graph.empty
        let tree = Relaxable.fromGraph "A" graph

        it "returns initial tree" $ do
          show tree `shouldBe` join ["A" <-- 0.0, "B" <-- "Nothing"]

    describe "relax" $ do
        context "one edge without cycles" $ do
            let graph = Graph.addEdge "A" "B" 10.0 .
                        Graph.addEdge "C" "B" 5.0 $ Graph.empty
            let tree = Relaxable.fromGraph "A" graph

            it "returns tree with relaxed edge is provided first time" $ do
              let ab = Edge.new "A" "B" 3.0
              let relaxedTree = Relaxable.relax ab tree

              show relaxedTree `shouldBe` join ["A" <-- 0.0,
                                                "B" <-- Edge.weight ab <-- "A",
                                                "C" <-- "Nothing"]

            context "and when one more shorter edge is specified" $ do
              let cb = Edge.new "C" "B" 5.0
              let ac = Edge.new "A" "C" 3.0
              let ab = Edge.new "A" "B" 3.0
              let relaxedTree = Relaxable.relax cb .
                                Relaxable.relax ac.
                                Relaxable.relax ab $ tree

              it "returns tree with edge relaxed one more time" $ do
                show relaxedTree `shouldBe` join ["A" <-- 0.0,
                                                  "B" <-- (Edge.weight ac + Edge.weight cb)  <-- "C",
                                                  "C" <-- Edge.weight ac <-- "A" ]

            context "and when longer edge is specified" $ do
              let ab0 = Edge.new "A" "B" 3.0
              let ab1 = Edge.new "A" "B" 2.0
              let relaxedTree = (Relaxable.relax ab1).
                                (Relaxable.relax ab0) $ tree

              it "returns non modified tree" $ do
                show relaxedTree `shouldBe` join ["A" <-- 0.0,
                                                  "B" <-- Edge.weight ab0 <-- "A",
                                                  "C" <-- "Nothing"]
