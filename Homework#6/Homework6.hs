module Homework6 where 

--Выразите функцию map :: (a -> b) -> [a] -> [b] используя функцию foldl.

map_foldl :: (a -> b) -> [a] -> [b]
map_foldl func list = foldl (\tmpList x -> tmpList ++ [(func x)]) [] list

--foldr
map_foldr :: (a -> b) -> [a] -> [b]
map_foldr func [] = []
map_foldr func (x:xs) = foldr (\y ys -> (func y):ys) [] xs


--Выразите функции filter :: (a -> Bool) -> [a] -> [a]

filter' :: (a->Bool) -> [a] -> [a]
filter' val [] = []
filter' func (x:xs) = case (func x) of
									True -> x:(filter' func xs)
									False ->(filter' func xs)

filter_foldr :: (a->Bool) -> [a] -> [a]
filter_foldr p []= []
filter_foldr p xs = foldr (\x xs -> if p x then x:xs else xs ) [] xs

filter_foldl :: (a->Bool) -> [a] -> [a]
filter_foldl p [] = []
filter_foldl p xs = foldl(\xs x -> if p x then xs ++ [x] else xs) [] xs

--concat :: [­[a]] -> [a]
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ (concat xs)

concat_foldr :: [[a]] -> [a]
concat_foldr xs = foldr (\x xs -> x ++ xs) [] xs

concat_foldl :: [[a]] -> [a]
concat_foldl xs = foldl (\xs x -> xs ++ x) [] xs

--concatMap :: (a -> [b]) -> [a] -> [b]
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' _ [] = []
concatMap' func (x:xs) = (func x) ++ (concatMap' func xs)

concatMap_foldr :: (a -> [b]) -> [a] -> [b]
concatMap_foldr p xs = foldr (\x xs -> (p x) ++ xs) [] xs

concatMap_foldl :: (a -> [b]) -> [a] -> [b]
concatMap_foldl p xs = foldl (\xs x -> xs ++ (p x)) [] xs
