module Task2 where

data DinnerOrder = Chicken | Pasta | Hungry
--interp. values of DinnerOrder

--example of function 
fn_for_dinner_order :: DinnerOrder -> a
fn_for_dinner_order v = case v of 
								Chicken -> undefined
								Pasta -> undefined
								Hungry -> undefined


dinner_order_to_message :: DinnerOrder -> String
dinner_order_to_message v = case v of
									Chicken -> "Passenger ordere chicken"
									Pasta -> "Passenger ordered pasta"
									Hungry -> "Passenger stays hungry"