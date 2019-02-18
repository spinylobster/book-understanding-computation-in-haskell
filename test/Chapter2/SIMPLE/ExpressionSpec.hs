module Chapter2.SIMPLE.ExpressionSpec (spec) where

import Test.Hspec
import Chapter2.SIMPLE.Expression
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  let reduceN n = foldl (.) reduce $ replicate (n-1) reduce

  describe "SIMPLE.Expression" $ do
    it "should be an instance of Show" $ do
      let exp = Add
            (Multiply (Number 1) (Number 2))
            (Multiply (Number 3) (Number 4))
      inspect exp `shouldBe` "<<1 * 2 + 3 * 4>>"
      let exp = Number 5
      inspect exp `shouldBe` "<<5>>"

    describe "reduce" $ do
      let env = Map.empty :: Env
      context ("with env: " <> show env) $ do
        let exp = Add
              (Multiply (Number 1) (Number 2))
              (Multiply (Number 3) (Number 4))
        describe (inspect exp) $ do
          let machine = (exp, env)

          let expected = Add (Number 2) (Multiply (Number 3) (Number 4))
          it ("reduce once should be: " <> show expected) $ do
            let actual = fst . reduce $ machine
            actual `shouldBe` expected

          let expected = Add (Number 2) (Number 12)
          it ("reduce twice should be: " <> show expected) $ do
            let actual = fst . (reduceN 2) $ machine
            actual `shouldBe` expected

          let expected = Number 14
          it ("reduce 3 times should be: " <> show expected) $ do
            let actual = fst . (reduceN 3) $ machine
            actual `shouldBe` expected
  
        let exp = LessThan (Number 5) (Add (Number 2) (Number 2))
        describe (inspect exp) $ do
          let machine = (exp, env)

          let expected = LessThan (Number 5) (Number 4)
          it ("reduce once should be: " <> show expected) $ do
            let actual = fst . reduce $ machine
            actual `shouldBe` expected

          let expected = Boolean False
          it ("reduce twice should be: " <> show expected) $ do
            let actual = fst . (reduceN 2) $ machine
            actual `shouldBe` expected

      let env = Map.fromList [("x", Number 3), ("y", Number 4)]
      context ("with env: " <> show env) $ do
        let exp = Add (Variable "x") (Variable "y")
        let machine = (exp, env)

        describe (inspect exp) $ do
          let expected = Add (Number 3) (Variable "y")
          it ("reduce once should be: " <> show expected) $ do
            let actual = fst . reduce $ machine
            actual `shouldBe` expected

          let expected = Add (Number 3) (Number 4)
          it ("reduce twice should be: " <> show expected) $ do
            let actual = fst . (reduceN 2) $ machine
            actual `shouldBe` expected

          let expected = Number 7
          it ("reduce 3 times should be: " <> show expected) $ do
            let actual = fst . (reduceN 3) $ machine
            actual `shouldBe` expected

    describe "evaluate" $ do
      let env = Map.empty :: Env
      context ("with env: " <> show env) $ do
        let exp = Number 23
        describe (inspect exp) $ do
          let machine = (exp, env)

          let expected = Number 23
          it ("evaluate should be: " <> show expected) $ do
            let actual = fst . evaluate $ machine
            actual `shouldBe` expected

      let env = Map.singleton "x" $ Number 23
      context ("with env: " <> show env) $ do
        let exp = Variable "x"
        describe (inspect exp) $ do
          let machine = (exp, env)

          let expected = Number 23
          it ("evaluate should be: " <> show expected) $ do
            let actual = fst . evaluate $ machine
            actual `shouldBe` expected

      let env = Map.fromList [("x", Number 2), ("y", Number 5)]
      context ("with env: " <> show env) $ do
        let exp = LessThan (Add (Variable "x") (Number 2)) (Variable "y")
        describe (inspect exp) $ do
          let machine = (exp, env)

          let expected = Boolean True
          it ("evaluate should be: " <> show expected) $ do
            let actual = fst . evaluate $ machine
            actual `shouldBe` expected

    describe "toRuby" $ do
      let exp = Number 5
      describe (inspect exp) $ do
        let expected = "-> e { 5 }"
        it ("should be: " <> expected) $ do
          let actual = toRuby exp
          actual `shouldBe` expected

      let exp = Boolean False
      describe (inspect exp) $ do
        let expected = "-> e { false }"
        it ("should be: " <> expected) $ do
          let actual = toRuby exp
          actual `shouldBe` expected

      let exp = Variable "x"
      describe (inspect exp) $ do
        let expected = "-> e { e[:x] }"
        it ("should be: " <> expected) $ do
          let actual = toRuby exp
          actual `shouldBe` expected

      let exp = Add (Variable "x") (Number 1)
      describe (inspect exp) $ do
        let expected = "-> e { (-> e { e[:x] }).call(e) + (-> e { 1 }).call(e) }"
        it ("should be: " <> expected) $ do
          let actual = toRuby exp
          actual `shouldBe` expected

      let exp = LessThan (Add (Variable "x") (Number 1)) (Number 3)
      describe (inspect exp) $ do
        let expected = "-> e { (-> e { (-> e { e[:x] }).call(e) + (-> e { 1 }).call(e) }).call(e) < (-> e { 3 }).call(e) }"

        it ("should be: " <> expected) $ do
          let actual = toRuby exp
          actual `shouldBe` expected
          
