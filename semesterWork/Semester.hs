module Semester where

import Data.Maybe


--implemention from Pierce's Types and Programming Languages
--http://port70.net/~nsz/articles/book/pierce_types_and_programming_languages_2002.pdf

--Представлением переменной является число - ее индекс де Брауна.
--Тип данных, представляющий абстрактные синтаксические деревья для термов, можно получить путем прямого перевода определения 6.1.2

--block with data definition

type Name = String

name_example = ""

fn_for_name :: Name -> a
fn_for_name t = (undefined t)


type DeBruijnIndex = Int
--synonym of Int 

--interp. indices of variable representation
start_deBruijnIndex = 0
random_deBruijnIndex = 12

fn_for_deBruijnIndex :: Int -> a
fn_for_deBruijnIndex t = (undefined t) 

type Info = String 
--synonym of String
info_example = ""

fn_for_info :: Info -> a
fn_for_info t = (undefined t)

-- используется тип данных context, type context = (string * binding) list
--которыи представляет собой простой список строк и соответствующих им связываний. Пока что связывания совершенно тривиальны
type Context = [String]

cont_example = ["a","b","c"]

context :: Context
context = ["x","y"]

empty_context :: Context
empty_context = []

fn_for_context :: Context -> a
fn_for_context [] = undefined 

data Term = Variable Name DeBruijnIndex | TmAbs Info Term | TmApp Term Term

--term_ex :: Term
--term_ex 

fn_for_term :: Term -> a
fn_for_term t = case t of 
					    Variable "" i -> undefined i
					    Variable name i -> undefined name
					    TmAbs name t1 -> undefined name t1
					    TmApp t1 t2 -> undefined t1 t2

pick_fresh_name :: Context -> Info -> (Context, Info)
pick_fresh_name ctx name
						| elem name ctx = pick_fresh_name ctx (name ++ "'")
						| otherwise = ((ctx ++ [name]), name)

print_for_term :: Context -> Term -> String
print_for_term ctx t = case t of 
						   TmAbs name v -> 
						   				let (ctx', name') = pick_fresh_name ctx name in "(lambda " ++ name' ++ ". " ++ (print_for_term ctx' v) ++ ")"
						   TmApp t1 t2 -> "(" ++ (print_for_term ctx t1) ++ " " ++ (print_for_term ctx t2) ++ ")"
						   Variable "" i -> if (i<(length ctx))
						   						then ctx !! i 
						   						else show i
						   Variable n i -> n

-- сдвиг и подстановка
--Внутреннийсдвиг↑dc (t)здесьпредставленвызовомвнутреннейфункции walk c t
walk :: DeBruijnIndex -> Term -> Int -> Term
walk i t v = case t of
					Variable "" i -> Variable "" (i+v)
					Variable f s -> Variable f s
					TmAbs name t1 -> TmAbs name (walk (i+1) t1 v)
					TmApp t1 t2 -> TmApp (walk i t1 v) (walk i t2 v)


term_shift :: Int -> Term -> Term 
term_shift d t = walk 0 t d 

term_subst :: DeBruijnIndex -> Term -> Term -> Term 
term_subst i part t = 
				    walk 0 t 
				    	where
				    		walk :: DeBruijnIndex -> Term -> Term
				    		walk a t = case t of 
				    						    Variable "" index -> if index == i 
				    						    							  then term_shift a part
				    						    							  else Variable "" index
				    						    Variable m k -> Variable m k
				    						    TmAbs name t1 -> TmAbs name (walk (a+1) t1)
				    						    TmApp t1 t2 -> TmApp (walk a t1) (walk a t2)	

-- терм, подставляемый вместо связанной переменной, сначала сдвигается на единицу вверх, а затем результат сдвигается на едини- цу вниз, чтобы отразить исчезновение использованной связанной пе- ременной
term_subst_top :: Term -> Term -> Term
term_subst_top ts t = term_shift (-1) (term_subst 0 (term_shift 1 ts) t)


--функция вычисления зависит от вспомогательного преди- ката
is_val :: Term -> Bool
is_val term = case term of
	TmAbs a b -> True
	v -> False
--Функция одношагового вычисления прямо кодирует правила вычисле- ния

evaluate1 :: Term -> Maybe Term
evaluate1 term = case term of
	TmApp fi t2 -> case fi of
		TmAbs _ v -> Just (term_subst_top t2 v)
		_ -> 
			if (is_val fi) 
				then
					let t2' = evaluate1 t2
					in case t2' of
						Just t' -> Just(TmApp fi t')
						Nothing -> Nothing
			else 
				let t1' = evaluate1 fi
				in case t1' of
					Just t' -> Just (TmApp t' t2)
					Nothing -> Nothing
	_ -> Nothing


eval :: Term -> Term 
eval t = let t' = evaluate1 t 
		 in case t' of 
		 	 		Just v -> eval v 
		 	 		Nothing -> t