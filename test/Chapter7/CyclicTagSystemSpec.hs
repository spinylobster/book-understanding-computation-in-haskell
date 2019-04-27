module Chapter7.CyclicTagSystemSpec (spec) where

import Test.Hspec
import Chapter7.CyclicTagSystem
import qualified Chapter7.TagSystem as T
import Data.Function ((&))

spec = do
  describe "CyclicTagSystem" $ do
    let stepN n system = foldl (&) system $ replicate n step

    it "step" $ do
      let rules = toRule <$> ["1", "0010", "10"]
      let system = CyclicTagSystem (toCyclicChar <$> "11") rules
      let system' = stepN 16 system
      currentString system' `shouldBe` (toCyclicChar <$> "00101")
      currentString (stepN 20 system') `shouldBe` (toCyclicChar <$> "101")

    let rulebook = T.TagRulebook 2 [T.TagRule 'a' "ccdd", T.TagRule 'b' "dd"]
    let system = T.TagSystem "aabbbb" rulebook

    it "alphabet" $ do
      alphabet system `shouldBe` "abcd"

    let encoder = calcEncoder system

    it "encodeString" $ do
      encoder `encodeString` "c" `shouldBe` (toCyclicChar <$> "0010")
      encoder `encodeString` "cab" `shouldBe` (toCyclicChar <$> "001010000100")

    it "cyclicRules" $ do
      let expected = toRule <$> ["0010001000010001", "00010001", "", ""]
      rulebook `cyclicRules` encoder `shouldBe` expected

    it "cyclicPaddingRules" $ do
      let expected = replicate 4 $ toRule ""
      rulebook `cyclicPaddingRules` encoder `shouldBe` expected

    it "toCyclic" $ do
      let system' = toCyclic system
      currentString system' `shouldBe` (toCyclicChar <$> "100010000100010001000100")
      let system'' = stepN 4 system'
      currentString system'' `shouldBe` (toCyclicChar <$> "100001000100010001000010001000010001")
      let system'' = stepN 24 system'
      currentString system'' `shouldBe` (toCyclicChar <$> "00100010000100010001000100010001")
