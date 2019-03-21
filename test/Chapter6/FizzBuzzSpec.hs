module Chapter6.FizzBuzzSpec (spec) where

import Test.Hspec
import Chapter6.FizzBuzz

spec :: Spec
spec =
  describe "lambda calculus" $ do
    it "fromEnum" $ do
      fromEnum zero `shouldBe` 0
      fromEnum three `shouldBe` 3
  
    it "toBoolean" $ do
      toBoolean true `shouldBe` True
      toBoolean false `shouldBe` False
    
    it "iF" $ do
      iF true "happy" "sad" `shouldBe` "happy"
      iF false "happy" "sad" `shouldBe` "sad"

    it "isZero" $ do
      toBoolean (isZero zero) `shouldBe` True
      toBoolean (isZero three) `shouldBe` False

    it "Pair" $ do
      let myPair = pair three five
      fromEnum (left myPair) `shouldBe` 3
      fromEnum (right myPair) `shouldBe` 5

    it "decrement" $ do
      fromEnum (decrement five) `shouldBe` 4
      fromEnum (decrement fifteen) `shouldBe` 14
      fromEnum (decrement hundred) `shouldBe` 99
      fromEnum (decrement zero) `shouldBe` 0

    it "isLessOrEqual" $ do
      toBoolean (isLessOrEqual one two) `shouldBe` True
      toBoolean (isLessOrEqual two two) `shouldBe` True
      toBoolean (isLessOrEqual three two) `shouldBe` False

    it "modN" $ do
      fromEnum (modN three two) `shouldBe` 1
      fromEnum (modN (power three three) (add three two)) `shouldBe` 2

    it "List" $ do
      let myList = unshift (unshift (unshift empty three) two) one
      fromEnum (first myList) `shouldBe` 1
      fromEnum (first (rest myList)) `shouldBe` 2
      fromEnum (first (rest (rest myList))) `shouldBe` 3
      toBoolean (isEmpty myList) `shouldBe` False
      toBoolean (isEmpty empty) `shouldBe` True
      (fromEnum <$> toList myList) `shouldBe` [1,2,3]

    it "range" $ do
      (fromEnum <$> toList (range one five)) `shouldBe` [1..5]

    it "fold" $ do
      fromEnum (fold (range one five) zero add) `shouldBe` 15
      fromEnum (fold (range one five) one multiply) `shouldBe` 120

    it "mapL" $ do
      (fromEnum <$> toList (mapL (range one five) increment)) `shouldBe` [2..6]

    it "Character" $ do
      toChar z `shouldBe` 'z'
      toString fizzbuzz `shouldBe` "FizzBuzz"

    it "toDigits" $ do
      toString (toDigits five) `shouldBe` "5"
      toString (toDigits (power five three)) `shouldBe` "125"

    it "solution'" $ do
      toString <$> (toList (solution' two four)) `shouldBe` ["2", "Fizz", "4"]
      let six = increment five
      toString <$> (toList (solution' four six)) `shouldBe` ["4", "Buzz", "Fizz"]
      let fourteen = decrement fifteen
      let sixteen = increment fifteen
      toString <$> (toList (solution' fourteen sixteen)) `shouldBe` ["14", "FizzBuzz", "16"]

    it "zeros" $ do
      fromEnum (first zeros) `shouldBe` 0
      fromEnum (first (rest zeros)) `shouldBe` 0
      fromEnum (first (rest (rest (rest (rest (rest zeros)))))) `shouldBe` 0
      take 5 (fromEnum <$> (toList zeros)) `shouldBe` replicate 5 0
      take 10 (fromEnum <$> (toList zeros)) `shouldBe` replicate 10 0
      take 20 (fromEnum <$> (toList zeros)) `shouldBe` replicate 20 0

    it "upwardsOf" $ do
      take 5 (fromEnum <$> (toList (upwardsOf zero))) `shouldBe` [0..4]
      take 20 (fromEnum <$> (toList (upwardsOf fifteen))) `shouldBe` [15..34]

    it "multiplesOf" $ do
      take 10 (fromEnum <$> (toList (multiplesOf two))) `shouldBe` [2,4..20]
      take 20 (fromEnum <$> (toList (multiplesOf five))) `shouldBe` [5,10..100]
      take 10 (fromEnum <$> (toList (multiplesOf three))) `shouldBe` [3,6..30]
      take 10 (fromEnum <$> (toList (mapL (multiplesOf three) increment))) `shouldBe` map (+1) [3,6..30]
      take 10 (fromEnum <$> (toList (mapL (multiplesOf three) (multiply two)))) `shouldBe` [6,12..60]

    it "multiplyStreams" $ do
      let actual = take 10 $ fromEnum <$> (toList (multiplyStreams (upwardsOf one) (multiplesOf three)))
      let expected = take 10 $ zipWith (*) [1..] [3,6..]
      actual `shouldBe` expected