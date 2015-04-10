module Task1 where

--htdd tasks
-- problem A
type OatmealTemp = Int
-- synonym of Int

--interp. value of oatmeal temperature

low_temperature, high_temperature, perfect_temperature :: OatmealTemp
-- constants
low_temperature = 0
high_temperature = 20
perfect_temperature = 10

--example of function
fn_for_oatmeal_temp :: OatmealTemp -> a
fn_for_oatmeal_temp v = (undefined v)

--problem B

data Adjustment = Left | Right | Current deriving (Show,Eq)
--interp. the value of adjustment

fn_for_adjustment :: Adjustment -> a
fn_for_adjustment l = case l of
							  Task1.Left -> undefined
							  Task1.Right -> undefined
							  Task1.Current -> undefined
-- problem C 
oatmeal_temp_to_adjustment :: OatmealTemp -> Adjustment
oatmeal_temp_to_adjustment v  = if v > perfect_temperature
	 							 then Task1.Left
	 						   else if v < perfect_temperature
	 						     then Task1.Right
	 						   else Task1.Current
-- oatmeal_temp_to_adjustment 5