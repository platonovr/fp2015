module TreeTask where

--Создать тип данных для бинарного дерева поиска (binary search tree), хранящего значения типа Int.



data BinaryTreeSearch = Nil | Cons Int BinaryTreeSearch BinaryTreeSearch deriving (Show,Eq)
--interp. binary search tree 

empty_tree :: BinaryTreeSearch
empty_tree = Nil


one_elem_of_tree :: BinaryTreeSearch
one_elem_of_tree = Cons 0 empty_tree empty_tree

--two_elem_of_tree


fn_for_tree :: BinaryTreeSearch -> a
fn_for_tree elem = case elem of 
						Nil -> undefined
						Cons leafValue left right -> undefined leafValue (fn_for_tree left) (fn_for_tree right)



--Реализовать функцию поиска заданного элемента в дереве.
get_tree_elem :: Int -> BinaryTreeSearch -> BinaryTreeSearch
get_tree_elem filter tree = case tree of 
									Nil -> Nil
									Cons leafValue left right -> if filter == leafValue
																		then tree
																	 else if filter > leafValue
																	 		then right
																	 else left 

--Реализовать функцию суммирования всех элементов в дереве.
get_tree_sum :: BinaryTreeSearch -> Int 
get_tree_sum tree = case tree of 
							 Nil -> 0
							 Cons leafValue left right -> leafValue + (get_tree_sum left) + (get_tree_sum right)


--Реализовать функцию подсчёта высоты этого дерева

get_tree_height :: BinaryTreeSearch -> Int
get_tree_height tree = case tree of 
							  Nil -> 0
							  Cons leafValue left right -> 1 + max (get_tree_height left) (get_tree_height right)