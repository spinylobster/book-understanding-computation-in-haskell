module Chapter7.PartialRecursiveFunctionSpec (spec) where

import Prelude hiding (subtract)
import Test.Hspec
import Chapter7.PartialRecursiveFunction

spec = do
  describe "PartialRecursiveFunction" $ do
    it "zero, increment" $ do
      zero `shouldBe` 0
      increment zero `shouldBe` 1
      increment (increment zero) `shouldBe` 2

    let two = increment (increment zero)
    let three = increment two
    let addThree x = increment (increment (increment x))
    it "define functions" $ do
      two `shouldBe` 2
      three `shouldBe` 3
      addThree two `shouldBe` 5

    let six = multiply two three
    it "recurse" $ do
      add two three `shouldBe` 5
      multiply two three `shouldBe` 6
      decrement six `shouldBe` 5
      subtract six two `shouldBe` 4
      subtract two six `shouldBe` 0

    it "minimize" $ do
      divide six two `shouldBe` 3
      let ten = increment (multiply three three)
      divide ten three `shouldBe` 3
      -- divide six zero

