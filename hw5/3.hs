lexLess :: (Ord a) => [a] -> [a] -> Bool
lexLess [] (_:_) = True 
lexLess (x:xs) (y:ys) = x < y || not (x > y) && lexLess xs ys
lexLess _ _ = False