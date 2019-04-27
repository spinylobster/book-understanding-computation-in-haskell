{-# LANGUAGE QuasiQuotes #-}

module Chapter7.SKICombinatorCalculus where

import Data.String.Interpolate
import Chapter6.LambdaCalculus (LCTerm(..))

data SKITerm
  = S | K | I
  | SKISymbol String
  | SKICall SKITerm SKITerm
  | Iota
  deriving (Eq)

instance Show SKITerm where
  show S = "S"
  show K = "K"
  show I = "I"
  show (SKISymbol name) = name
  show (SKICall left right) = [i|#{left}[#{right}]|]
  show Iota = "ι"

call :: SKITerm -> [SKITerm] -> SKITerm
S `call` [a, b, c] = SKICall (SKICall a c) (SKICall b c)
K `call` [a, b] = a
I `call` [a] = a
Iota `call` [a] = SKICall (SKICall a S) K

combinator :: SKITerm -> SKITerm
combinator (SKICall left _) = combinator left
combinator self = self

arguments :: SKITerm -> [SKITerm]
arguments (SKICall left right) = arguments left ++ [right]
arguments _ = []

callable :: SKITerm -> [SKITerm] -> Bool
S `callable` arguments = length arguments == 3
K `callable` arguments = length arguments == 2
I `callable` arguments = length arguments == 1
Iota `callable` arguments = length arguments == 1
_ `callable` _ = False

reducible :: SKITerm -> Bool
reducible self@(SKICall left right) = reducible left || reducible right || combinator self `callable` arguments self
reducible _ = False

reduce :: SKITerm -> SKITerm
reduce self@(SKICall left right)
  = if reducible left then SKICall (reduce left) right
  else if reducible right then SKICall left (reduce right)
  else combinator self `call` arguments self

asAFunctionOf :: SKITerm -> String -> SKITerm
self@(SKISymbol name) `asAFunctionOf` name' = if name == name' then I else SKICall K self
(SKICall left right) `asAFunctionOf` name = SKICall (SKICall S leftFunction) rightFunction
  where leftFunction = left `asAFunctionOf` name
        rightFunction = right `asAFunctionOf` name
self `asAFunctionOf` _ = SKICall K self

toSki :: LCTerm -> SKITerm
toSki (LCV name) = SKISymbol name
toSki (LCC left right) = SKICall (toSki left) (toSki right)
toSki (LCF param body) = (toSki body) `asAFunctionOf` param

toIota :: SKITerm -> SKITerm
toIota S = SKICall Iota (SKICall Iota (SKICall Iota (SKICall Iota Iota)))
toIota K = SKICall Iota (SKICall Iota (SKICall Iota Iota))
toIota I = SKICall Iota Iota
toIota (SKICall left right) = SKICall (toIota left) (toIota right)
toIota self = self
