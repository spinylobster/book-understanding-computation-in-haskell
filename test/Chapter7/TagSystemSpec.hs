module Chapter7.TagSystemSpec (spec) where

import Test.Hspec
import Chapter7.TagSystem

spec = do
  describe "TagSystem" $ do
    it "step" $ do
      let rulebook = TagRulebook 2 [TagRule 'a' "aa", TagRule 'b' "bbbb"]
      let system = TagSystem "aabbbbbb" rulebook
      currentString system `shouldBe` "aabbbbbb"

      let system' = step system
      currentString system' `shouldBe` "bbbbbbaa"

      let system'' = step system'
      currentString system'' `shouldBe` "bbbbaabbbb"

      let system''' = step system''
      currentString system''' `shouldBe` "bbaabbbbbbbb"

      let system'''' = step system'''
      currentString system'''' `shouldBe` "aabbbbbbbbbbbb"

    it "run" $ do
      let rulebook = TagRulebook 2 [TagRule 'a' "cc", TagRule 'b' "dddd"]
      let system = TagSystem "aabbbbbb" rulebook
      run system `shouldBe` "ccdddddddddddd"

    it "halve" $ do
      let rulebook = TagRulebook 2 [TagRule 'a' "cc", TagRule 'b' "d"]
      let system = TagSystem "aabbbbbbbbbbbb" rulebook
      run system `shouldBe` "ccdddddd"

    it "increment" $ do
      let rulebook = TagRulebook 2 [TagRule 'a' "ccdd", TagRule 'b' "dd"]
      let system = TagSystem "aabbbb" rulebook
      run system `shouldBe` "ccdddddd"

    it "double and increment" $ do
      let rulebook = TagRulebook 2
            [ TagRule 'a' "cc", TagRule 'b' "dddd"
            , TagRule 'c' "eeff", TagRule 'd' "ff"]
      let system = TagSystem "aabbbb" rulebook
      run system `shouldBe` "eeffffffffff"

    it "judge even or odd" $ do
      let rulebook = TagRulebook 2
            [ TagRule 'a' "cc", TagRule 'b' "d"
            , TagRule 'c' "eo", TagRule 'd' ""
            , TagRule 'e' "e"]
      let system = TagSystem "aabbbbbbbb" rulebook
      run system `shouldBe` "e"

      let system = TagSystem "aabbbbbbbbbb" rulebook
      run system `shouldBe` "o"
