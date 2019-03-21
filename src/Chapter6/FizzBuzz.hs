{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BlockArguments #-}

module Chapter6.FizzBuzz where

import Prelude hiding (subtract)
import Unsafe.Coerce

newtype Number = N (forall a. (a -> a) -> a -> a)

instance Enum Number where
  fromEnum (N f) = f succ 0
  toEnum 0 = zero
  toEnum n | n > 0 = increment $ toEnum (n-1)

zero = N \f -> \x -> x
one = N \f -> \x -> f x
two = N \f -> \x -> f (f x)
three = add one two
four = add two two
five = add two three
ten = multiply two five
fifteen = multiply five three
hundred = power ten two

newtype Boolean = B (forall a. a -> a -> a)

toBoolean :: Boolean -> Bool
toBoolean (B f) = f True False

true = B \t -> \f -> t
false = B \t -> \f -> f

iF = \(B f) -> \cons -> \alt -> f cons alt

isZero = \(N n) -> n (\n -> false) true

newtype Pair x y = P (forall c. (x -> y -> c) -> c)

pair = \x -> \y -> P \f -> f x y
left = \(P p) -> p (\x -> \y -> x)
right = \(P p) -> p (\x -> \y -> y)

increment = \(N n) -> N $ \f -> \x -> f (n f x)
decrement = \(N n) -> left (n slide (pair zero zero))
  where slide = \p -> pair (right p) (increment (right p))
add = \(N m) -> \(N n) -> N $ \f -> \x -> m f (n f x)
subtract = \m -> \(N n) -> n decrement m
multiply = \m -> \(N n) -> n (add m) zero
power = \m -> \(N n) -> n (multiply m) one

isLessOrEqual :: Number -> Number -> Boolean
isLessOrEqual = \m -> \n -> isZero (subtract m n)

y = \f -> (\x -> f (x (unsafeCoerce x))) (\x -> f (x (unsafeCoerce x)))

modN = y \f -> \m -> \n -> iF (isLessOrEqual n m) (f (subtract m n) n) m

newtype List a = L (Pair Boolean (Pair a (List a)))

toList :: List a -> [a]
toList l = iF (isEmpty l) [] (first l : toList (rest l))

empty = L $ pair true (unsafeCoerce true)
unshift = \l -> \x -> L $ pair false (pair x l)
isEmpty = \(L l) -> left l
first = \(L l) -> left (right l)
rest = \(L l) -> right (right l)

range = y \f -> \m -> \n -> iF (isLessOrEqual m n) (unshift (f (increment m) n) m) empty

fold = y \f -> \l -> \x -> \g -> iF (isEmpty l) x (g (f (rest l) x g) (first l))

mapL = \k -> \f -> fold k empty (\l -> \x -> unshift l (f x))

type Character = Number

toChar :: Character -> Char
toChar n = "0123456789BFiuz" !! (fromEnum n)
toString :: List Character -> String
toString l = toChar <$> (toList l)

b = ten
f = increment b
i = increment f
u = increment i
z = increment u
fizz = unshift (unshift (unshift (unshift empty z) z) i) f
buzz = unshift (unshift (unshift (unshift empty z) z) u) b
fizzbuzz = unshift (unshift (unshift (unshift buzz z) z) i) f

divN = y \f -> \m -> \n -> iF (isLessOrEqual n m) (increment (f (subtract m n) n)) zero
push = \l -> \x -> fold l (unshift empty x) unshift
toDigits = y \f -> \n -> push (iF (isLessOrEqual n nine) empty (f (divN n ten))) (modN n ten)
  where nine = decrement ten

solution' = \m -> \n -> mapL (range m n) (\n ->
  iF (isZero (modN n fifteen)) fizzbuzz (
    iF (isZero (modN n three)) fizz (
      iF (isZero (modN n five)) buzz (
        toDigits n))))
solution = solution' one hundred

zeros = y (\f -> unshift f zero)
upwardsOf = y (\f -> \n -> unshift (f (increment n)) n)
multiplesOf = \m -> y (\f -> \n -> unshift (f (add m n)) n) m
multiplyStreams = y \f -> \k -> \l -> unshift (f (rest k) (rest l)) (multiply (first k) (first l))

-- modN = \m@(N m') -> \n -> m' (\x -> iF (isLessOrEqual n x) (subtract x n) x) m
-- countdown = \p -> pair (unshift (left p) (right p)) (decrement (right p))
-- range = \m -> \n -> left ((count m n) countdown (pair empty n))
--   where count m n = let (N c) = increment (subtract n m) in c
