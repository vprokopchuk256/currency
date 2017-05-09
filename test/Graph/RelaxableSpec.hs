module Graph.RelaxableSpec (main, spec) where

import Test.Hspec

import qualified Data.HashMap.Strict as Map
import qualified Graph.Relaxable as Relaxable

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

            context "when has nodes with distance info" $ do
                let tree = show $ Relaxable.Tree (Map.fromList [("A", ("B", 123.45))])
                it "it returns string with routing information" $ do
                    tree `shouldBe` "\"A\" <- 123.45 <- \"B\""
        describe "cycle" $ do
            context "when empty" $ do
                let cycle = show $ (Relaxable.Cycle [] 0.0 :: Relaxable.Relaxable String)
                it "it returns empty string" $ do
                    cycle `shouldBe` ""
            context "when has nodes" $ do
                let cycle = show $ (Relaxable.Cycle ["A", "B"] 0.45 :: Relaxable.Relaxable String)
                it "it returns string with routing information" $ do
                    cycle `shouldBe` "\"A\" <- \"B\" (0.45)"

