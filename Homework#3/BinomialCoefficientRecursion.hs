module BinomialCoefficientRecursion where

bin n 0 = 1
bin 0 k = 0
bin n k = bin (n-1) (k-1) * n `div` k