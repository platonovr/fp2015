module TreeTask2 where


--http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types
--Создать тип данных для бинарного дерева, хранящего в листьях элементы произвольного типа.

data BinaryTree a = Nil | Cons a (BinaryTree a) (BinaryTree a) deriving (Show,Eq,Read)
--interp. Binary tree which has leaves with any types

empty_tree :: BinaryTree a
empty_tree = Nil

--one_elem_of_tree :: BinaryTree a
--one_elem_of_tree = Cons 0 empty_tree empty_tree

fn_for_tree :: BinaryTree a -> b
fn_for_tree elem = case elem of 
						Nil -> undefined
						Cons leafValue right left -> undefined leafValue (fn_for_tree left) (fn_for_tree right)

-- Реализовать функцию подсчёта высоты этого дерева.

get_tree_height :: BinaryTree a -> Int 
get_tree_height tree = case tree of 
								Nil -> 0
								Cons leafValue left right -> max (get_tree_height left) (get_tree_height right)

--Реализовать функцию tmap, принимающюю дерево и функцию преобразования элемента (a -> b) и возвращающую новое дерево той же формы, но содержащее элементы, полученные применением функции к исходным элементам

tmap :: BinaryTree a -> (a->b) -> BinaryTree b
tmap tree func = case tree of
						Nil -> Nil
						Cons leafValue left right -> Cons (func leafValue) (tmap left func) (tmap right func)