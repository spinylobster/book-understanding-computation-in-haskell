module Chapter7.SKICombinatorCalculusSpec (spec) where

import Prelude hiding (subtract)
import Test.Hspec
import Chapter7.SKICombinatorCalculus
import Chapter6.LambdaCalculus (parseLambda)

spec = do
  describe "SKICombinator" $ do
    it "should be an instance of Show" $ do
      let x = SKISymbol "x"
      show x `shouldBe` "x"
      let expression = SKICall (SKICall S K) (SKICall I x)
      show expression `shouldBe` "S[K][I[x]]"

    it "call" $ do
      let [x, y, z] = SKISymbol <$> ["x", "y", "z"]
      S `call` [x, y, z] `shouldBe` SKICall (SKICall x z) (SKICall y z)

    it "combinator, arguments" $ do
      let [x, y, z] = SKISymbol <$> ["x", "y", "z"]
      let expression = SKICall (SKICall (SKICall S x) y) z
      combinator expression `shouldBe` S
      arguments expression `shouldBe` [x, y, z]
      combinator expression `call` arguments expression `shouldBe` S `call` [x, y, z]
    
    it "callable" $ do
      let [x, y, z] = SKISymbol <$> ["x", "y", "z"]
      let expression = SKICall x (SKICall y z)
      combinator expression `callable` arguments expression  `shouldBe` False
      let expression = SKICall S (SKICall x y)
      combinator expression `callable` arguments expression  `shouldBe` False
      let expression = SKICall (SKICall (SKICall S x) y) z
      combinator expression `callable` arguments expression  `shouldBe` True

    it "reduce" $ do
      let swap = SKICall (SKICall S (SKICall K (SKICall S I))) K
      let [x, y] = SKISymbol <$> ["x", "y"]
      let expression = SKICall (SKICall swap x) y
      let expression' = reduce expression
      expression' `shouldBe` SKICall (SKICall (SKICall (SKICall K (SKICall S I)) x) (SKICall K x)) y
      let expression'' = reduce expression'
      expression'' `shouldBe` SKICall (SKICall (SKICall S I) (SKICall K x)) y
      let expression''' = reduce expression''
      expression''' `shouldBe` SKICall (SKICall I y) (SKICall (SKICall K x) y)
      let expression'''' = reduce expression'''
      expression'''' `shouldBe` SKICall y (SKICall (SKICall K x) y)
      let expression''''' = reduce expression''''
      expression''''' `shouldBe` SKICall y x

    it "asAFunctionOf" $ do
      let original = SKICall (SKICall S K) I
      let function = original `asAFunctionOf` "x"
      function `shouldBe` SKICall (SKICall S (SKICall (SKICall S (SKICall K S)) (SKICall K K))) (SKICall K I)
      reducible function `shouldBe` False

      let reduceN n = foldl (.) id $ replicate n reduce
      let y = SKISymbol "y"
      let expression = SKICall function y
      reduceN 5 expression `shouldBe` original

      let x = SKISymbol "x"
      let original = SKICall (SKICall S x) I
      let function = original `asAFunctionOf` "x"
      let expression = SKICall function y
      reduceN 5 expression `shouldBe` SKICall (SKICall S y) I
      reduceN 5 expression == original `shouldBe` False

    it "toSki" $ do
      let (Right two) = parseLambda "-> p { -> x { p[p[x]] } }"
      let [inc, zero] = SKISymbol <$> ["inc", "zero"]
      let expression = SKICall (SKICall (toSki two) inc) zero
      let reduceN n = foldl (.) id $ replicate n reduce
      reduceN 18 expression `shouldBe` SKICall inc (SKICall inc zero)

    it "toIota" $ do
      let expression = toIota S
      let reduceN n = foldl (.) id $ replicate n reduce
      reduceN 12 expression `shouldBe` S

      let expression = toIota K
      reduceN 10 expression `shouldBe` K

      let expression = toIota I
      let identity = SKICall (SKICall S K) (SKICall K K)
      reduceN 3 expression `shouldBe` identity

      let x = SKISymbol "x"
      let expression = SKICall identity x
      reduceN 3 expression `shouldBe` x

      let (Right two) = parseLambda "-> p { -> x { p[p[x]] } }"
      let [inc, zero] = SKISymbol <$> ["inc", "zero"]
      let expression = SKICall (SKICall (toIota (toSki two)) inc) zero
      reduceN 199 expression `shouldBe` SKICall inc (SKICall inc zero)
