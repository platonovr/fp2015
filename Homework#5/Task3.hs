module Task3 where


data PairList a b = LNil | First { firstValue :: a
									, next :: (PairList a b)}
                     | Second { secondValue :: b
                             , next :: (PairList a b) }


empty_lopl::PairList a b 
empty_lopl = LNil 

lopl_length :: PairList a b -> Int
lopl_length  LNil = 0 
lopl_length lopl =  1 + lopl_length (next lopl) 