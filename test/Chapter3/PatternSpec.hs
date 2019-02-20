module Chapter3.PatternSpec (spec) where

import Test.Hspec
import Chapter3.NFA
import Chapter3.Pattern
import qualified Text.Parsec as P

spec :: Spec
spec =
  describe "Pattern" $ do
    it "should be an instance of Show" $ do
      let pattern = Repeat $ Choose
            (Concatenate (Literal 'a') (Literal 'b'))
            (Literal 'a')
      inspect pattern `shouldBe` "/(ab|a)*/"

    describe "toNfa" $ do
      describe "everyPattern" $ do
        describe "Empty" $ do
          let actual = toNfa Empty
          let expected = makeNfa [1] [1] []
          it ("should be " <> show expected) $ actual `shouldBe` expected

        describe "Literal" $ do
          let actual = toNfa $ Literal 'a'
          let expected = makeNfa [1] [2] [nfaRule (1, Just 'a') 2]
          it ("should be " <> show expected) $ actual `shouldBe` expected

        describe "Concatenate" $ do
          let actual = toNfa $ Concatenate (Literal 'a') (Literal 'b')
          let rulebook = [nfaRule (1, Just 'a') 2, nfaRule (2, Just 'b') 3]
          let expected = makeNfa [1] [3] rulebook
          it ("should be " <> show expected) $ actual `shouldBe` expected

        describe "Choose" $ do
          let actual = toNfa $ Choose (Literal 'a') (Literal 'b')
          let rulebook = [nfaRule (2, Just 'a') 3, nfaRule (4, Just 'b') 5
                        , nfaRule (1, Nothing) 2, nfaRule (1, Nothing) 4
                        , nfaRule (3, Nothing) 6, nfaRule (5, Nothing) 6]
          let expected = makeNfa [1] [6] rulebook
          it ("should be " <> show expected) $ actual `shouldBe` expected

        describe "Repeat" $ do
          let actual = toNfa $ Repeat $ Literal 'a'
          let rulebook = [nfaRule (1, Just 'a') 2, nfaRule (1, Nothing) 3, nfaRule (2, Nothing) 1]
          let expected = makeNfa [1] [3] rulebook
          it ("should be " <> show expected) $ actual `shouldBe` expected

    describe "matches" $ do
      it "Empty" $ Empty `matches` "a" `shouldBe` False

      it "Literal" $ Literal 'a' `matches` "a" `shouldBe` True

      let pat = Concatenate (Literal 'a') (Concatenate (Literal 'b') (Literal 'c'))
      it "Concatenate" $ do
        pat `matches` "a" `shouldBe` False
        pat `matches` "ab" `shouldBe` False
        pat `matches` "abc" `shouldBe` True

      let pat = Choose (Literal 'a') (Literal 'b')
      it "Choose" $ do
        pat `matches` "a" `shouldBe` True
        pat `matches` "b" `shouldBe` True
        pat `matches` "c" `shouldBe` False

      let pat = Repeat (Literal 'a')
      it "Repeat" $ do
        pat `matches` "" `shouldBe` True
        pat `matches` "a" `shouldBe` True
        pat `matches` "aaaa" `shouldBe` True
        pat `matches` "b" `shouldBe` False

      let pat = Repeat $ Concatenate (Literal 'a') (Choose Empty (Literal 'b'))
      it (inspect pat) $ do
        pat `matches` "" `shouldBe` True
        pat `matches` "a" `shouldBe` True
        pat `matches` "ab" `shouldBe` True
        pat `matches` "aba" `shouldBe` True
        pat `matches` "abab" `shouldBe` True
        pat `matches` "abaab" `shouldBe` True
        pat `matches` "abba" `shouldBe` False

    describe "parsePattern" $ do
      let expected = Right $ Repeat $ Concatenate (Literal 'a') (Choose Empty (Literal 'b')) :: Either P.ParseError Pattern
      let patStr = "(a(|b))*"
      let actual = parsePattern patStr
      it ("parse \"" <> patStr <> "\" should be " <> show expected) $ do
        actual `shouldBe` expected
