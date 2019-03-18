module Chapter4.DPDASpec (spec) where

import Test.Hspec
import Chapter3.Automaton
import Chapter4.DPDA

spec :: Spec
spec =
  describe "DPDA" $ do
    describe "accepts" $ do
      let rulebook = [pdaRule (1, '$', Just '(') ("b$", 2), pdaRule (2, 'b', Just '(') ("bb", 2)
                    , pdaRule (2, 'b', Just ')') ("", 2), pdaRule (2, '$', Nothing) ("$", 1)]
      let dpda = makeDpda 1 [1] rulebook
      describe (show dpda) $ do
        it "" $ do
          dpda `accepts` "(((((((((())))))))))" `shouldBe` True
          dpda `accepts` "()(())((()))(()(()))" `shouldBe` True
          dpda `accepts` "(()(()(()()(()()))()" `shouldBe` False
          dpda `accepts` "())" `shouldBe` False

      let rulebook = [pdaRule (1, '$', Just 'a') ("a$", 2), pdaRule (1, '$', Just 'b') ("b$", 2)
                    , pdaRule (2, 'a', Just 'a') ("aa", 2), pdaRule (2, 'b', Just 'b') ("bb", 2)
                    , pdaRule (2, 'b', Just 'a') ("", 2), pdaRule (2, 'a', Just 'b') ("", 2)
                    , pdaRule (2, '$', Nothing) ("$", 1)]
      let dpda = makeDpda 1 [1] rulebook
      describe (show dpda) $ do
        it "" $ do
          dpda `accepts` "ababab" `shouldBe` True
          dpda `accepts` "bbbaaaab" `shouldBe` True
          dpda `accepts` "baa" `shouldBe` False

      let rulebook = [pdaRule (1, '$', Just 'a') ("a$", 1), pdaRule (1, 'a', Just 'a') ("aa", 1)
                    , pdaRule (1, 'b', Just 'a') ("ab", 1), pdaRule (1, '$', Just 'b') ("b$", 1)
                    , pdaRule (1, 'a', Just 'b') ("ba", 1), pdaRule (1, 'b', Just 'b') ("bb", 1)
                    , pdaRule (1, '$', Just 'm') ("$", 2), pdaRule (1, 'a', Just 'm') ("a", 2)
                    , pdaRule (1, 'b', Just 'm') ("b", 2), pdaRule (2, 'a', Just 'a') ("", 2)
                    , pdaRule (2, 'b', Just 'b') ("", 2), pdaRule (2, '$', Nothing) ("$", 3)]
      let dpda = makeDpda 1 [3] rulebook
      describe (show dpda) $ do
        it "" $ do
          dpda `accepts` "abmba" `shouldBe` True
          dpda `accepts` "babbamabbab" `shouldBe` True
          dpda `accepts` "abmb" `shouldBe` False
          dpda `accepts` "baambaa" `shouldBe` False
