module Chapter2.SIMPLE.ParserSpec (spec) where

import Test.Hspec
import Chapter2.SIMPLE
import Chapter2.SIMPLE.Parser
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "SIMPLE.parseSimple" $ do
    let source = "x = 1; while (x < 5) { x = x * 3 }"
    describe source $ do
      it "x should be 9" $ do
        let Right stat = parseSimple source
        let resultEnv = snd . evaluate $ (stat, Map.empty)
        resultEnv `shouldBe` Map.singleton "x" (Number 9)
