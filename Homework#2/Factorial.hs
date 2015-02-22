module Factorial where

fac :: Integer -> Integer
fac 1 = 1
fac x = x * fac(x-1)
