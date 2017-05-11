module StrSpec (main, spec) where

import Test.Hspec

import Str

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "str" $ do
    it "outputs floats" $ do
      str 5.0 `shouldBe` "5.0"
    it "outputs strings without quoting" $ do
      str "str" `shouldBe` "str"
  describe "<--" $ do
    it "concatenates two strings" $ do
      ("str1" <-- "str2") `shouldBe` "str1 <- str2"
    it "concatenates string and float" $ do
      ("str1" <-- 15.0) `shouldBe` "str1 <- 15.0"
    it "concatenates string float and string again" $ do
      ("str1" <-- 15.0 <-- "str2") `shouldBe` "str1 <- 15.0 <- str2"

