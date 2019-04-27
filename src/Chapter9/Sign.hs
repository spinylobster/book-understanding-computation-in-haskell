module Chapter9.Sign where

import Chapter2.SIMPLE

data Sign = Negative | Zero | Positive | Unknown
  deriving (Show, Eq)

instance Num Sign where
  _ * Zero = Zero
  Zero * _ = Zero
  _ * Unknown = Unknown
  Unknown * _ = Unknown
  a * b | a == b = Positive
  _ * _ = Negative

  a + b | a == b = a
  a + Zero = a
  Zero + b = b
  _ + _ = Unknown

  abs Negative = Positive

  signum Positive = 1
  signum Zero = 0
  signum Negative = -1

  fromInteger = sign
  negate a = a * Negative

sign :: Integer -> Sign
sign n | n < 0 = Negative
sign 0 = Zero
sign n = Positive

calculate :: Num a => a -> a -> a -> a
calculate x y z = (x * y) * (x * z)

instance Ord Sign where
  a <= b = a == b || b == Unknown

sumOfSquares x y = (x * x) + (y * y)
