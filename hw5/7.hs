withWindow :: [a] -> Integer -> [[a]]

withWindow l windowSize = helper l windowSize [] []
  where helper :: [a] -> Integer -> [a] -> [[a]] -> [[a]]
        helper [] windowSize acc result = if windowSize > 0 then [] else reverse (acc:result)
        helper (y:ys) 0 (x:xs) result = helper ys 0 (xs ++ [y]) ((x:xs):result)
        helper (x:xs) windowSize acc result = helper xs (windowSize-1) (acc ++ [x]) result