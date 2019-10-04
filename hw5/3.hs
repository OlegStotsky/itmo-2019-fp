lexLess :: (Ord a) => [a] -> [a] -> Bool
lexLess (x:xs) (y:ys) = x < y || not (x == y) && lexLess xs ys
lexLess [] (_:_) = True 
lexLess _ _ = False