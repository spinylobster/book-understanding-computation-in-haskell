{-# LANGUAGE QuasiQuotes #-}

module Chapter4.AnalyseSpec (spec) where

import Test.Hspec
import Chapter3.Automaton
import Chapter4.Analyse

import Data.String.Interpolate

spec :: Spec
spec = do
  let test calcActual input expected =
        let actual = calcActual input
        in it [i|#{input} should be #{expected}|] $ actual `shouldBe` expected
  describe "analyse" $ do
    let test' = test analyse
    test' "y = x * 7" "v=v*n"
    test' "while (x < 5) { x = x * 3 }" "w(v<n){v=v*n}"
    test' "if (x < 10) { y = true; x = 0 } else { do-nothing }" "i(v<n){v=b;v=n}e{d}"

  describe "syntaxAnalyzer" $ do
    let test' = test $ (syntaxAnalyzer `accepts`) . analyse
    test' "while (x < 5) { x = x * 3 }" True
    test' "while (x < 5) { x = x * }" False
