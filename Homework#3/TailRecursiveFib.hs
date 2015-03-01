module TailRecursiveFib where

fib :: Integer -> Integer
fib n = fib' n 1 1
    where
      fib' 0 r p = r
      fib' n r p = fib' (n-1) p (r+p)