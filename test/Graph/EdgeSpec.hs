module Graph.EdgeSpec (main, spec) where

import Test.Hspec

import qualified Graph.Edge as Edge

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "new" $ do
    it "just works" $ do
      "to" `shouldBe` "to"
