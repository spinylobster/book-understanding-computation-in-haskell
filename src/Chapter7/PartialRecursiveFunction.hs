{-# LANGUAGE ViewPatterns #-}

module Chapter7.PartialRecursiveFunction where

import Prelude hiding (subtract)

zero = 0
increment = (+ 1)

recurse :: ([Integer] -> b) -> (Integer -> b -> [Integer] -> b) -> [Integer] -> b
recurse f g values = if lastValue == zero then valueWhenZero else easing
  where (reverse -> lastValue : (reverse -> otherValues)) = values
        valueWhenZero = f otherValues
        easing = g easierY easierResult otherValues
          where easierY = pred lastValue
                easierValues = otherValues ++ [easierY]
                easierResult = recurse f g easierValues

add x y = recurse addZeroToX incrementEasierResult [x, y]
  where
    addZeroToX (x:_) = x
    incrementEasierResult _ easierResult _ = increment easierResult

multiply x y = recurse multiplyXByZero addXToEisierResult [x, y]
  where
    multiplyXByZero _ = zero
    addXToEisierResult _ easierResult (x:_) = add x easierResult

decrement x = recurse (const zero) easierX [x]
  where
    easierX x _ _ = x

subtract x y = recurse subtractZeroFromX decrementEasierResult [x, y]
  where
    subtractZeroFromX (x:_) = x
    decrementEasierResult _ easierResult _ = decrement easierResult

minimize f = minimize' 0 f
  where minimize' n f = if f n == zero then n else minimize' (n + 1) f

divide x y = minimize f
  where f n = subtract (increment x) (multiply y (increment n))
