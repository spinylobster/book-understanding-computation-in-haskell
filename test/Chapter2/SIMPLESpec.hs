module Chapter2.SIMPLESpec (spec) where

import Test.Hspec
import Chapter2.SIMPLE
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

  describe "SIMPLE.parseSimple" $ do
    let source = "x = 1; while (x < 5) { x = x * 3 }"
    describe source $ do
      it "x should be 9" $ do
        let Right stat = parseSimple source
        let resultEnv = snd . evaluate $ (stat, Map.empty)
        resultEnv `shouldBe` Map.singleton "x" (Number 9)
