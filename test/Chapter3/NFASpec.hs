module Chapter3.NFASpec (spec) where

import Test.Hspec
import Chapter3.Automaton
import Chapter3.DFA
import Chapter3.NFA

spec :: Spec
spec =
  describe "NFA" $ do
    describe "accepting" $ do
      let nfa = makeNfa [1] [4] []
      let expected = False
      it ("should be " <> show expected) $ accepting nfa `shouldBe` expected

      let nfa = makeNfa [1, 2, 4] [4] []
      let expected = True
      it ("should be " <> show expected) $ accepting nfa `shouldBe` expected

    let rulebook = [nfaRule (1, Just 'a') 1, nfaRule (1, Just 'b') 1, nfaRule (1, Just 'b') 2
                  , nfaRule (2, Just 'a') 3, nfaRule (2, Just 'b') 3
                  , nfaRule (3, Just 'a') 4, nfaRule (3, Just 'b') 4]
    let nfa = makeNfa [1] [4] rulebook
    describe (show nfa) $ do
      describe "readString" $ do
        let expected = makeNfa [1, 2, 4] [4] rulebook
        it "" $ nfa `readString` "bab" `shouldBe` expected
  
        let expected = makeNfa [1, 2, 3, 4] [4] rulebook
        it "" $ nfa `readString` "bbbbb" `shouldBe` expected
  
      describe "accepts" $ do
        it "" $ nfa `accepts` "bab" `shouldBe` True
        it "" $ nfa `accepts` "bbbbb" `shouldBe` True
        it "" $ nfa `accepts` "bbabb" `shouldBe` False

    let rulebook = [nfaRule (1, Nothing) 2, nfaRule (1, Nothing) 4
                  , nfaRule (2, Just 'a') 3, nfaRule (3, Just 'a') 2
                  , nfaRule (4, Just 'a') 5, nfaRule (5, Just 'a') 6, nfaRule (6, Just 'a') 4]
    let nfa = makeNfa [1] [2, 4] rulebook
    describe (show nfa) $ do
      describe "accepts" $ do
        it "" $ nfa `accepts` "aa" `shouldBe` True
        it "" $ nfa `accepts` "aaa" `shouldBe` True
        it "" $ nfa `accepts` "aaaaa" `shouldBe` False
        it "" $ nfa `accepts` "aaaaaa" `shouldBe` True

    describe "toDfa" $ do
      let rulebook = [nfaRule (1, Just 'a') 1, nfaRule (1, Just 'a') 2, nfaRule (1, Nothing) 2
                  , nfaRule (2, Just 'b') 3
                  , nfaRule (3, Just 'b') 1, nfaRule (3, Nothing) 2]
      let nfa = makeNfa [1] [3] rulebook
      let actual = toDfa nfa
      let dfaRulebook = [ dfaRule (2, 'a') 2, dfaRule (1, 'a') 1
                        , dfaRule (3, 'a') 1, dfaRule (4, 'a') 2
                        , dfaRule (2, 'b') 2, dfaRule (1, 'b') 4
                        , dfaRule (3, 'b') 3, dfaRule (4, 'b') 3]
      let expected = makeDfa 1 [3, 4] dfaRulebook
      it ("should be " <> show expected) $ actual `shouldBe` expected

      it "" $ do
        let dfa = actual
        dfa `accepts` "aaa" `shouldBe` False
        dfa `accepts` "aab" `shouldBe` True
        dfa `accepts` "bbbabb" `shouldBe` True
