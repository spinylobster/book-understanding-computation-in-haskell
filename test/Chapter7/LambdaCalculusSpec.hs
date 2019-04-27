module Chapter7.LambdaCalculusSpec (spec) where

import Test.Hspec
import Chapter6.FizzBuzz
import Chapter7.LambdaCalculus

spec :: Spec
spec = do
  describe "LambdaCalculus" $ do
    it "Tape" $ do
      let currentTape = tape empty zero empty zero
      let currentTape' = tapeWrite currentTape one
      let currentTape'' = tapeMoveHeadRight currentTape'
      let currentTape''' = tapeWrite currentTape'' two
      let currentTape'''' = tapeMoveHeadRight currentTape'''
      let currentTape''''' = tapeWrite currentTape'''' three
      let currentTape'''''' = tapeMoveHeadRight currentTape'''''

      fromEnum <$> toList (tapeLeft currentTape'''''') `shouldBe` [1,2,3]
      fromEnum (tapeMiddle currentTape'''''') `shouldBe` 0
      fromEnum <$> toList (tapeRight currentTape'''''') `shouldBe` []
