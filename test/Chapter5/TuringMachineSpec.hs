{-# LANGUAGE QuasiQuotes #-}

module Chapter5.TuringMachineSpec (spec) where

import Test.Hspec
import Data.String.Interpolate
import Chapter5.TuringMachine

spec :: Spec
spec = do
  describe "Tape" $ do
    let tape = mkRightStartTape "1011"

    let expected = "_101(1)_"
    it ("shows like " ++ expected) $ show tape `shouldBe` expected

    let expected = Tape (Just <$> "01") (Just '1') [Just '1']
    it ("move left should be " ++ show expected) $ tape `moveHead` DLeft `shouldBe` expected

    let expected = Tape (Just <$> "101") (Just '0') []
    it ("write '0' should be " ++ show expected) $ tape `write` (Just '0') `shouldBe` expected

    let expected = Tape (Just <$> "1101") Nothing []
    it ("move right should be " ++ show expected) $ tape `moveHead` DRight `shouldBe` expected

    let expected = Tape (Just <$> "1101") (Just '0') []
    it ("move right and write '0' should be " ++ show expected) $
      let tape' = tape `moveHead` DRight
      in let tape'' = tape' `write` Just '0'
      in tape'' `shouldBe` expected

  describe "TMRule" $ do
    describe "appliesTo" $ do
      let rule = TMRule 1 (Just '0') 2 (Just '1') DRight
      let test input expected = it [i|#{rule} appliesTo #{input} should be #{expected}|] $
            rule `appliesTo` input `shouldBe` expected

      test (1, Tape [] (Just '0') []) True
      test (1, Tape [] (Just '1') []) False
      test (2, Tape [] (Just '0') []) False

    describe "follow" $ do
      let rule = TMRule 1 (Just '0') 2 (Just '1') DRight
      let config = (1, Tape [] (Just '0') [Nothing])
      let expected = (2, Tape [Just '1'] Nothing [])
      it [i|#{rule} follow #{config} should be #{expected}|] $
            rule `follow` config `shouldBe` expected

  describe "DTMRulebook" $
    describe "nextConfig" $ do
      let rules =
            [ TMRule 1 (Just '0') 2 (Just '1') DRight, TMRule 1 (Just '1') 1 (Just '0') DLeft
            , TMRule 1 Nothing 2 (Just '1') DRight, TMRule 2 (Just '0') 2 (Just '0') DRight
            , TMRule 2 (Just '1') 2 (Just '1') DRight, TMRule 2 Nothing 3 Nothing DLeft
            ]

      let config = (1, mkRightStartTape "1011")
      let config' = (1, Tape [Just '0', Just '1'] (Just '1') [Just '0'])
      it [i|nextConfig #{config} should be #{config'}|] $ rules `nextConfig` config `shouldBe` config'

      let config'' = (1, Tape [Just '1'] (Just '0') [Just '0', Just '0'])
      it [i|nextConfig #{config'} should be #{config''}|] $ rules `nextConfig` config' `shouldBe` config''

      let config''' = (2, Tape [Just '1', Just '1'] (Just '0') [Just '0'])
      it [i|nextConfig #{config''} should be #{config'''}|] $ rules `nextConfig` config'' `shouldBe` config'''

  describe "DTM" $ do
    describe "binaries incrementer" $ do
      let rules =
            [ TMRule 1 (Just '0') 2 (Just '1') DRight, TMRule 1 (Just '1') 1 (Just '0') DLeft
            , TMRule 1 Nothing 2 (Just '1') DRight, TMRule 2 (Just '0') 2 (Just '0') DRight
            , TMRule 2 (Just '1') 2 (Just '1') DRight, TMRule 2 Nothing 3 Nothing DLeft
            ]
  
      describe "happy path" $ do
        let config = (1, mkRightStartTape "1011")
        let dtm = DTM config [3] rules
        it ("first config: " ++ show config) $ currentConfig dtm `shouldBe` config
        it ("the DTM is not accepting") $ accepting dtm `shouldBe` False
    
        let dtm' = step dtm
        let config = (1, Tape [Just '0', Just '1'] (Just '1') [Just '0'])
        it ("next config: " ++ show config) $ currentConfig dtm' `shouldBe` config
        it ("the DTM is not accepting yet") $ accepting dtm' `shouldBe` False
    
        let dtm'' = run dtm'
        let config = (3, Tape [Just '0', Just '1', Just '1'] (Just '0') [])
        it ("final config: " ++ show config) $ currentConfig dtm'' `shouldBe` config
        it ("the DTM is accepting now") $ accepting dtm'' `shouldBe` True
  
      describe "stuck path" $ do
        let config = (1, mkRightStartTape "1211")
        let dtm = DTM config [3] rules
        it ("first config: " ++ show config) $ currentConfig dtm `shouldBe` config
  
        let dtm' = run dtm
        let config = (-1, Tape [] (Just '1') [])
        it ("final config: " ++ show config) $ currentConfig dtm' `shouldBe` config
        it ("the DTM is not accepting") $ accepting dtm' `shouldBe` False

    describe "'abc' counter" $ do
      let rules =
            -- 状態1: aを探して右にスキャンする
            [ TMRule 1 (Just 'X') 1 (Just 'X') DRight, TMRule 1 (Just 'a') 2 (Just 'X') DRight
            , TMRule 1 Nothing 6 Nothing DLeft
            -- 状態2: bを探して右にスキャンする
            , TMRule 2 (Just 'a') 2 (Just 'a') DRight, TMRule 2 (Just 'X') 2 (Just 'X') DRight
            , TMRule 2 (Just 'b') 3 (Just 'X') DRight
            -- 状態3: cを探して右にスキャンする
            , TMRule 3 (Just 'b') 3 (Just 'b') DRight, TMRule 3 (Just 'X') 3 (Just 'X') DRight
            , TMRule 3 (Just 'c') 4 (Just 'X') DRight
            -- 状態4: 文字列の末尾を探して右にスキャンする
            , TMRule 4 (Just 'c') 4 (Just 'c') DRight, TMRule 4 Nothing 5 Nothing DLeft
            -- 状態5: 文字列の先頭を探して左にスキャンする
            , TMRule 5 (Just 'a') 5 (Just 'a') DLeft, TMRule 5 (Just 'b') 5 (Just 'b') DLeft
            , TMRule 5 (Just 'c') 5 (Just 'c') DLeft, TMRule 5 (Just 'X') 5 (Just 'X') DLeft
            , TMRule 5 Nothing 1 Nothing DRight
            ]

      let config = (1, mkLeftStartTape "aaabbbccc")
      let dtm = DTM config [6] rules
      it ("first config: " ++ show config) $ currentConfig dtm `shouldBe` config
  
      let stepN n dtm = if n > 0 then stepN (n-1) $ step dtm else dtm

      let dtm' = stepN 10 dtm
      let config = (5, mkRightStartTape "XaaXbbXcc")
      it ("config after 10 steps: " ++ show config) $ currentConfig dtm' `shouldBe` config

      let dtm'' = stepN 25 dtm'
      let config = (5, Tape (Just <$> "aXX") (Just 'X') (Just <$> "XbXXc"))
      it ("config after more 25 steps: " ++ show config) $ currentConfig dtm'' `shouldBe` config

      let dtm''' = run dtm''
      let config = (6, mkRightStartTape "XXXXXXXXX")
      it ("final config: " ++ show config) $ currentConfig dtm''' `shouldBe` config
