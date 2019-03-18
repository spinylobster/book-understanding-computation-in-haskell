module Chapter4.NPDASpec (spec) where

import Test.Hspec
import Chapter3.Automaton
import Chapter4.DPDA (pdaRule)
import Chapter4.NPDA

spec :: Spec
spec =
  describe "NPDA" $ do
    let rulebook = [pdaRule (1, '$', Just 'a') ("a$", 1), pdaRule (1, 'a', Just 'a') ("aa", 1)
                  , pdaRule (1, 'b', Just 'a') ("ab", 1), pdaRule (1, '$', Just 'b') ("b$", 1)
                  , pdaRule (1, 'a', Just 'b') ("ba", 1), pdaRule (1, 'b', Just 'b') ("bb", 1)
                  , pdaRule (1, '$', Nothing) ("$", 2), pdaRule (1, 'a', Nothing) ("a", 2)
                  , pdaRule (1, 'b', Nothing) ("b", 2), pdaRule (2, 'a', Just 'a') ("", 2)
                  , pdaRule (2, 'b', Just 'b') ("", 2), pdaRule (2, '$', Nothing) ("$", 3)]
    let npda = makeNpda [3] rulebook
    describe (show npda) $ do
      it "accepts" $ do
        npda `accepts` "abba" `shouldBe` True
        npda `accepts` "babbaabbab" `shouldBe` True
        npda `accepts` "abb" `shouldBe` False
        npda `accepts` "baabaa" `shouldBe` False
