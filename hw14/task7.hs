import Control.Monad.State

dropFirstAndInsertLast :: a -> [a] -> [a]
dropFirstAndInsertLast x [xs] = [x]
dropFirstAndInsertLast x [] = [x]
dropFirstAndInsertLast elem (x:xs) = xs ++ [elem]

tail' :: StateT [String] IO ()
tail' = do line <- lift $ getLine
           st <- get
           if (length st) > 10 then 
             do put $ dropFirstAndInsertLast line st
           else
             do return ()