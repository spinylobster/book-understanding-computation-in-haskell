module Chapter6.LambdaCalculusSpec (spec) where

import Test.Hspec
import Chapter6.LambdaCalculus

spec :: Spec
spec = do
  describe "LambdaCalculus" $ do
    let one = LCF "p" (LCF "x" (LCC (LCV "p") (LCV "x")))
    let increment = LCF "n" (LCF "p" (LCF "x" (LCC (LCV "p") (LCC (LCC (LCV "n") (LCV "p")) (LCV "x")))))
    let add = LCF "m" (LCF "n" (LCC (LCC (LCV "n") increment) (LCV "m")))

    it "show" $ do
      show one `shouldBe` "-> p { -> x { p[x] } }"
      show increment `shouldBe` "-> n { -> p { -> x { p[n[p][x]] } } }"
      show add `shouldBe` "-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }"

    it "replace" $ do
      let x = LCV "x"
      let f = LCF "y" (LCV "y")
      x `replace` ("x", f) `shouldBe` f
      x `replace` ("z", f) `shouldBe` x

      let c = LCC (LCC (LCC (LCV "a") (LCV "b")) (LCV "c")) (LCV "b")
      c `replace` ("a", LCV "x") `shouldBe` LCC (LCC (LCC (LCV "x") (LCV "b")) (LCV "c")) (LCV "b")
      let f = LCF "x" (LCV "x")
      c `replace` ("b", f) `shouldBe` LCC (LCC (LCC (LCV "a") f) (LCV "c")) f

      let f = LCF "y" (LCC (LCV "x") (LCV "y"))
      f `replace` ("x", LCV "z") `shouldBe` LCF "y" (LCC (LCV "z") (LCV "y"))
      f `replace` ("y", LCV "z") `shouldBe` f

      let c = LCC (LCC (LCV "x") (LCV "y")) (LCF "y" (LCC (LCV "y") (LCV "x")))
      let expected = LCC (LCC (LCV "z") (LCV "y")) (LCF "y" (LCC (LCV "y") (LCV "z")))
      c `replace` ("x", LCV "z") `shouldBe` expected
      let expected = LCC (LCC (LCV "x") (LCV "z")) (LCF "y" (LCC (LCV "y") (LCV "x")))
      c `replace` ("y", LCV "z") `shouldBe` expected

    it "call" $ do
      let function = LCF "x" (LCF "y" (LCC (LCV "x") (LCV "y")))
      let argument = LCF "z" (LCV "z")
      function `call` argument `shouldBe` LCF "y" (LCC (LCF "z" (LCV "z")) (LCV "y"))

    it "reduce" $ do
      let expression = LCC (LCC add one) one
      let (inc, zero) = (LCV "inc", LCV "zero")
      let reduceN n = foldl (.) id $ replicate n reduce
      let expression' = LCC (LCC (reduceN 5 expression) inc) zero

      let reduced = reduce expression'
      let expected = LCC (LCF "x" (LCC inc (LCC (LCC (LCF "p" (LCF "x" (LCC (LCV "p") (LCV "x")))) inc) (LCV "x")))) zero
      reduced `shouldBe` expected

      let reduced' = reduce reduced
      let expected = (LCC inc (LCC (LCC (LCF "p" (LCF "x" (LCC (LCV "p") (LCV "x")))) inc) zero))
      reduced' `shouldBe` expected

      let reduced'' = reduce reduced'
      let expected = (LCC inc (LCC (LCF "x" (LCC inc (LCV "x"))) zero))
      reduced'' `shouldBe` expected

      let reduced''' = reduce reduced''
      let expected = LCC inc (LCC inc zero)
      reduced''' `shouldBe` expected

    it "parse" $ do
      let str = "-> x { x[x] }[-> y { y }]"
      let expected = Right $ LCC (LCF "x" (LCC (LCV "x") (LCV "x"))) (LCF "y" (LCV "y"))
      parseLambda str `shouldBe` expected

      let expected = Right $ LCC (LCF "y" (LCV "y")) (LCF "y" (LCV "y"))
      reduce <$> parseLambda str `shouldBe` expected
