{-# LANGUAGE QuasiQuotes #-}

module Chapter9.SignSpec (spec) where

import Test.Hspec
import Chapter9.Sign

import Data.String.Interpolate
import qualified Data.Set as Set

spec = do
  describe "(*)" $ do
    let test a b result = it [i|#{a} * #{b} should be #{result}|] $ a * b `shouldBe` result
    test Positive Positive Positive
    test Negative Zero Zero
    test Positive Negative Negative

  describe "sign" $ do
    let test a result = it [i|sign #{a} should be #{result}|] $ sign a `shouldBe` result
    test 6 Positive
    test (-9) Negative
    it "sign 6 * sign (-9) should be Negative" $ sign 6 * sign (-9) `shouldBe` Negative

  describe "calculate" $ do
    let test x y z result = it [i|calculate #{x} #{y} #{z} should be #{result}|] $
          calculate x y z `shouldBe` result
    test 3 (-5) 0 0
    test Positive Negative Zero Zero
  
  describe "safety of Sign" $ do
    it "sign (6 * (-9)) == sign 6 * sign (-9)" $
      sign (6 * (-9)) `shouldBe` sign 6 * sign (-9)
    it "sign (100 * 0) == sign 100 * sign 0" $
      sign (100 * 0) `shouldBe` sign 100 * sign 0
    it "calculate 1 (-2) 3 == calculate (sign 1) (sign (-2)) (sign 3)" $
      sign (calculate 1 (-2) 3) `shouldBe` calculate (sign 1) (sign (-2)) (sign 3)

  describe "(+)" $ do
    let test a b result = it [i|#{a} + #{b} should be #{result}|] $ a + b `shouldBe` result
    test Positive Positive Positive
    test Negative Zero Negative
    test Negative Positive Unknown
    test Positive Unknown Unknown
    test Unknown Zero Unknown
    it "Positive + Negative + Negative should be Unknown" $
      Positive + Negative + Negative `shouldBe` Unknown
  
  it "(Positive + Negative) * Zero + Positive should be Positive" $
    (Positive + Negative) * Zero + Positive `shouldBe` Positive

  describe "safety of Sign 2" $ do
    it "sign (10 + 3) == sign 10 + sign 3" $
      sign (10 + 3) `shouldBe` sign 10 + sign 3
    it "sign (-5 + 0) == sign (-5) + sign 0" $
      sign (-5 + 0) `shouldBe` sign (-5) + sign 0
    it "sign (6 + (-9)) /= sign 6 + sign (-9)" $
      sign (6 + (-9)) `shouldNotBe` sign 6 + sign (-9)
    it "sign (6 + (-9)) == Negative" $
      sign (6 + (-9)) `shouldBe` Negative
    it "sign 6 + sign (-9) == Unknown" $
      sign 6 + sign (-9) `shouldBe` Unknown
  
  describe "<=" $ do
    let test a b result = it [i|#{a} <= #{b} should be #{result}|] $ a <= b `shouldBe` result
    test Positive Positive True
    test Positive Unknown True
    test Positive Negative False

    it "sign (6 * (-9)) <= sign 6 * sign (-9)" $
      sign (6 * (-9)) <= sign 6 * sign (-9) `shouldBe` True
    it "sign (-5 + 0) <= sign (-5) * sign 0" $
      sign (-5 + 0) <= sign (-5) + sign 0 `shouldBe` True
    it "sign (6 + (-9)) <= sign 6 + sign (-9)" $
      sign (6 + (-9)) <= sign 6 + sign (-9) `shouldBe` True

  it "sumOfSquares" $ do
    let inputs = [Negative, Zero, Positive]
    let result = [sumOfSquares x y | x <- inputs, y <- inputs]
    Set.fromList result `shouldBe` Set.fromList [Positive, Zero]
