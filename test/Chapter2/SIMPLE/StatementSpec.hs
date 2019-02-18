module Chapter2.SIMPLE.StatementSpec (spec) where

import Test.Hspec
import Chapter2.SIMPLE.Expression
import Chapter2.SIMPLE.Statement
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  let reduceN n = foldl (.) reduce $ replicate (n-1) reduce

  describe "SIMPLE.Statement" $ do
    it "should be an instance of Show" $ do
      let stat = Assign "x" $ Add (Variable "x") (Number 1)
      inspect stat `shouldBe` "<<x = x + 1>>"

    describe "reduce" $ do
      let env = Map.singleton "x" $ Number 2
      context ("with env: " <> show env) $ do
        let stat = Assign "x" $ Add (Variable "x") (Number 1)
        describe (inspect stat) $ do
          let machine = (stat, env)

          let expected = (Assign "x" $ Add (Number 2) (Number 1), env)
          it ("reduce once should be: " <> show expected) $ do
            let actual = reduce machine
            actual `shouldBe` expected

          let expected = (Assign "x" $ Number 3, env)
          it ("reduce twice should be: " <> show expected) $ do
            let actual = reduceN 2 machine
            actual `shouldBe` expected

          let env' = Map.singleton "x" $ Number 3
          let expected = (DoNothing, env')
          it ("reduce 3 times should be: " <> show expected) $ do
            let actual = reduceN 3 machine
            actual `shouldBe` expected
  
      let env = Map.singleton "x" $ Boolean True
      let stat = If (Variable "x") (Assign "y" (Number 1)) (Assign "y" (Number 2))
      context ("with env: " <> show env) $ do
        describe (inspect stat) $ do
          let machine = (stat, env)

          let expected = (If (Boolean True) (Assign "y" (Number 1)) (Assign "y" (Number 2)), env)
          it ("reduce once should be: " <> show expected) $ do
            let actual = reduce machine
            actual `shouldBe` expected

          let expected = (Assign "y" (Number 1), env)
          it ("reduce twice should be: " <> show expected) $ do
            let actual = reduceN 2 machine
            actual `shouldBe` expected

          let env' = Map.fromList [("x", Boolean True), ("y", Number 1)]
          let expected = (DoNothing, env')
          it ("reduce 3 times should be: " <> show expected) $ do
            let actual = reduceN 3 machine
            actual `shouldBe` expected
  
      let env = Map.empty :: Env
      let onePlusOne = Add (Number 1) (Number 1)
      let xPlusThree = Add (Variable "x") (Number 3)
      let stat = Sequence (Assign "x" onePlusOne) (Assign "y" xPlusThree)
      context ("with env: " <> show env) $ do
        describe (inspect stat) $ do
          let machine = (stat, env)

          let expected = (Sequence (Assign "x" $ Number 2) (Assign "y" xPlusThree), env)
          it ("reduce once should be: " <> show expected) $ do
            let actual = reduce machine
            actual `shouldBe` expected

          let env' = Map.singleton "x" $ Number 2
          let expected = (Sequence DoNothing (Assign "y" xPlusThree), env')
          it ("reduce twice should be: " <> show expected) $ do
            let actual = reduceN 2 machine
            actual `shouldBe` expected

          let expected = (Assign "y" xPlusThree, env')
          it ("reduce 3 times should be: " <> show expected) $ do
            let actual = reduceN 3 machine
            actual `shouldBe` expected

          let expected = (Assign "y" $ Add (Number 2) (Number 3), env')
          it ("reduce 4 times should be: " <> show expected) $ do
            let actual = reduceN 4 machine
            actual `shouldBe` expected

          let expected = (Assign "y" $ Number 5, env')
          it ("reduce 5 times should be: " <> show expected) $ do
            let actual = reduceN 5 machine
            actual `shouldBe` expected

          let env' = Map.fromList [("x", Number 2), ("y", Number 5)]
          let expected = (DoNothing, env')
          it ("reduce 6 times should be: " <> show expected) $ do
            let actual = reduceN 6 machine
            actual `shouldBe` expected

      let env = Map.singleton "x" $ Number 1
      let cond = LessThan (Variable "x") (Number 5)
      let body = Assign "x" (Multiply (Variable "x") (Number 3))
      let stat = While cond body
      context ("with env: " <> show env) $ do
        describe (inspect stat) $ do
          let machine = (stat, env)

          let expected = (If (Boolean True) (Sequence body stat) DoNothing, env)
          it ("reduce 3 times should be: " <> show expected) $ do
            let actual = reduceN 3 machine
            actual `shouldBe` expected

          let expected = (Sequence (Assign "x" (Number 3)) stat, env)
          it ("reduce 6 times should be: " <> show expected) $ do
            let actual = reduceN 6 machine
            actual `shouldBe` expected

          let env' = Map.singleton "x" $ Number 3
          let expected = (stat, env')
          it ("reduce 8 times should be: " <> show expected) $ do
            let actual = reduceN 8 machine
            actual `shouldBe` expected

          let env' = Map.singleton "x" $ Number 9
          let expected = (DoNothing, env')
          it ("reduce 20 times should be: " <> show expected) $ do
            let actual = reduceN 20 machine
            actual `shouldBe` expected
 
    describe "evaluate" $ do
      let env = Map.empty :: Env
      context ("with env: " <> show env) $ do
        let onePlusOne = Add (Number 1) (Number 1)
        let xPlusThree = Add (Variable "x") (Number 3)
        let stat = Sequence (Assign "x" onePlusOne) (Assign "y" xPlusThree)
        describe (inspect stat) $ do
          let machine = (stat, env)

          let expected = (DoNothing, Map.fromList [("x", Number 2), ("y", Number 5)])
          it ("evaluate should be: " <> show expected) $ do
            let actual = evaluate $ machine
            actual `shouldBe` expected

      let env = Map.singleton "x" $ Number 1
      let cond = LessThan (Variable "x") (Number 5)
      let body = Assign "x" (Multiply (Variable "x") (Number 3))
      let stat = While cond body
      context ("with env: " <> show env) $ do
        describe (inspect stat) $ do
          let machine = (stat, env)

          let expected = (DoNothing, Map.singleton "x" $ Number 9)
          it ("evaluate should be: " <> show expected) $ do
            let actual = evaluate $ machine
            actual `shouldBe` expected

    describe "toRuby" $ do
      let stat = Assign "y" $ Add (Variable "x") (Number 1)
      describe (inspect stat) $ do
        let expected = "-> e { e.merge({ :y => (-> e { (-> e { e[:x] }).call(e) + (-> e { 1 }).call(e) }).call(e) }) }"
        it ("should be: " <> expected) $ do
          let actual = toRuby stat
          actual `shouldBe` expected

      let stat = DoNothing
      describe (inspect stat) $ do
        let expected = "-> e { e }"
        it ("should be: " <> expected) $ do
          let actual = toRuby stat
          actual `shouldBe` expected

      let env = Map.singleton "x" $ Number 1
      let cond = LessThan (Variable "x") (Number 5)
      let body = Assign "x" (Multiply (Variable "x") (Number 3))
      let stat = While cond body
      describe (inspect stat) $ do
        let expected = "-> e { while (-> e { (-> e { e[:x] }).call(e) < (-> e { 5 }).call(e) }).call(e); e = (-> e { e.merge({ :x => (-> e { (-> e { e[:x] }).call(e) * (-> e { 3 }).call(e) }).call(e) }) }).call(e); end; e }" 
        it ("should be: " <> expected) $ do
          let actual = toRuby stat
          actual `shouldBe` expected
