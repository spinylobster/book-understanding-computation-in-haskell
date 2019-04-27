{-# LANGUAGE QuasiQuotes #-}

module Chapter9.TypeSpec (spec) where

import Test.Hspec
import Chapter9.Type
import Chapter2.SIMPLE

import qualified Data.Map.Strict as Map
import Data.String.Interpolate

spec = do
  describe "Type" $ do
    let test ctx exp result = it [i|"type of #{exp} should be #{result}"|] $
          evalType ctx exp `shouldBe` result

    describe "Expression" $ do
      let ctx = Map.empty :: Context
      test ctx (Add (Number 1) (Number 2)) NumberType
      test ctx (Add (Number 1) (Boolean True)) Error
      test ctx (LessThan (Number 1) (Number 2)) BooleanType
      test ctx (LessThan (Number 1) (Boolean True)) Error

      describe "Variable" $ do
        let exp = Add (Variable "x") (Variable "y")

        let ctx = Map.empty :: Context
        context (show ctx) $ do
          test ctx exp Error

        let ctx = Map.fromList [("x", NumberType), ("y", NumberType)]
        context (show ctx) $ do
          test ctx exp NumberType

        let ctx = Map.fromList [("x", NumberType), ("y", BooleanType)]
        context (show ctx) $ do
          test ctx exp Error

    describe "Statement" $ do
      let ctx = Map.empty
      test ctx (If (LessThan (Number 1) (Number 2)) DoNothing DoNothing) Void
      test ctx (If (Add (Number 1) (Number 2)) DoNothing DoNothing) Error
      
      let ctx = Map.singleton "x" BooleanType
      let exp = While (Variable "x") DoNothing
      context (show ctx) $ do
        test ctx exp Void
      
      let ctx = Map.singleton "x" NumberType
      context (show ctx) $ do
        test ctx exp Error
      
      let exp = While (LessThan (Variable "x") (Number 5)) (Assign "x" (Add (Variable "x") (Number 3)))
      let ctx = Map.empty :: Context
      context (show ctx) $ do
        test ctx exp Error

      let ctx = Map.singleton "x" NumberType
      context (show ctx) $ do
        test ctx exp Void

      let ctx = Map.singleton "x" BooleanType
      context (show ctx) $ do
        test ctx exp Error