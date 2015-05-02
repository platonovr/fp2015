module TypedArithTask where

--implemention from Pierce's Types and Programming Languages

--good example of lambda calculus interpeter -https://code.google.com/p/tapl-haskell/source/checkout

--block with data definition

type NameOfType = String 
--synonym of string
name_of_type_example :: NameOfType
name_of_type_example = "exmpl"

fn_for_name_of_type :: NameOfType -> a
fn_for_name_of_type t = (undefined t)

--Синтаксис типов прямо переводится из абстрактного синтаксиса, пред- ставленного на рис. 8.1 и 9.1, в определение типа языка ML.
data Type = TpNotValid String| TpBool | TpNamed NameOfType | TpArr Type Type deriving (Show,Eq)
-- i saw how to throw exceptions in haskell, but i used specified TpNotValid type if description of e
-- interp. a type of possible terms

type_example :: Type
type_example = TpNamed "dt"

fn_for_type :: Type -> a
fn_for_type t = case t of 
				TpNotValid description -> undefined description
				TpBool -> undefined
				TpNamed aName -> undefined aName
				TpArr type1 type2 -> undefined type1 type2


--Представление термов такое же, как было у нас при реализации бестипового лямбда-исчисления (с. 85), но с добавлением аннотации типа к варианту TmAbs.
data Term = TmTrue | TmFalse | TmIf Term Term Term 
			 | Variable Int Type | TmAbs String Type Term | TmApp Term Term deriving (Show,Eq)


--interp. a term of labmda calculation
term_example :: Term
term_example = Variable (0) (TpNamed "q")

fn_for_term :: Term -> a
fn_for_term t = case t of 
					  TmTrue -> undefined
					  TmFalse -> undefined
					  TmIf t1 t2 t3 -> undefined t1 t2 t3
					  Variable ind aType -> undefined ind aType
					  TmAbs info aType t1 -> undefined info aType t1 
					  TmApp t1 t2 -> undefined t1 t2 


--Функцию проверки типов typeof можно рассматривать как простой пе- ревод правил типизации для λ→ (рис. 8.1 и 9.1) или как перевод леммы об инверсии (9.3.1)
typeof :: Term -> Type 
typeof t = case t of
				TmTrue -> TpBool
				TmFalse -> TpBool
				TmIf t1 t2 t3 -> if (typeof t1) == TpBool then
										let t2Type = typeof t2 in 
										if t2Type == (typeof t3)  then t2Type
											else TpNotValid "conditional and alternative of an if statement must be of the same type"
								 else TpNotValid "guard of conditional not a boolean"
				Variable ind aType -> aType
				TmAbs v aType term -> TpArr aType (typeof term)
				TmApp t1 t2 -> let t1Type = typeof t1 in
							   let t2Type = typeof t2 in
							   case t1Type of
							   			TpArr tyT11 tyT12 -> if t2Type == tyT11 
							   									then tyT12
							   									else TpNotValid "param type mismatch"
							   			_ -> TpNotValid "arrow type expected"

