module LibSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  describe "test" $ do
    it "should be pass" $ do
      "hoge" `shouldBe` "hoge"
