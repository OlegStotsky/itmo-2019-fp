last' :: [a] -> a
last' [] = error "empty list"
last' [a] = a
last' (x:xs) = seq x last' x