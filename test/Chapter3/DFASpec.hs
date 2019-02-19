module Chapter3.DFASpec (spec) where

import Test.Hspec
import Chapter3.Automaton
import Chapter3.DFA

spec :: Spec
spec =
  describe "DFA" $ do
    let rulebook = [dfaRule (1, 'a') 2, dfaRule (1, 'b') 1
                  , dfaRule (2, 'a') 2, dfaRule (2, 'b') 3
                  , dfaRule (3, 'a') 3, dfaRule (3, 'b') 3]

    describe "accepting" $ do
      let dfa = makeDfa 1 [1, 3] []
      it "should be True" $ accepting dfa `shouldBe` True

      let dfa = makeDfa 1 [3] []
      it "should be False" $ accepting dfa `shouldBe` False

    describe "readCharacter" $ do
      let dfa = makeDfa 1 [3] rulebook

      let expected = makeDfa 1 [3] rulebook
      it "" $ dfa `readCharacter` 'b' `shouldBe` expected

      let expected = makeDfa 2 [3] rulebook
      it "" $ dfa `readCharacter` 'a' `shouldBe` expected

    describe "readString" $ do
      let dfa = makeDfa 1 [3] rulebook

      let expected = makeDfa 3 [3] rulebook
      it "" $ dfa `readString` "baaab" `shouldBe` expected

    describe "accepts" $ do
      let dfa = makeDfa 1 [3] rulebook

      it "" $ dfa `accepts` "a" `shouldBe` False
      it "" $ dfa `accepts` "baa" `shouldBe` False
      it "" $ dfa `accepts` "baba" `shouldBe` True
