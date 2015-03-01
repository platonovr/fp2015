module TailBinomialCoefficient where

bin :: Integer -> Integer -> Integer
bin n k = bin' 1 n k 1 1
 	where 
 		bin' 1 n 0 r p = r `div` p
 		bin' 1 n k r p = bin' 1 (n-1) (k-1) (r*n) (k*p)