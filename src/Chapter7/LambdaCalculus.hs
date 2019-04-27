module Chapter7.LambdaCalculus where

import Chapter6.FizzBuzz

newtype Tape = T (Pair (Pair (List Number) Number) (Pair (List Number) Number))
tape = \l -> \m -> \r -> \b -> T $ pair (pair l m) (pair r b)
tapeLeft = \(T t) -> left (left t)
tapeMiddle = \(T t) -> right (left t)
tapeRight = \(T t) -> left (right t)
tapeBlank = \(T t) -> right (right t)

tapeWrite = \t -> \c -> tape (tapeLeft t) c (tapeRight t) (tapeBlank t)

tapeMoveHeadRight = \t -> tape (left t) (middle t) (right t) (tapeBlank t)
  where left = \t -> push (tapeLeft t) (tapeMiddle t)
        middle = \t -> iF (isEmpty (tapeRight t)) (tapeBlank t) (first (tapeRight t))
        right = \t -> iF (isEmpty (tapeRight t)) empty (rest (tapeRight t))
