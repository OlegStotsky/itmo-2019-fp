data IntTree = Leaf | Node Int [IntTree]

sumIntTree :: IntTree -> Integer
sumIntTree (Leaf) = 0
sumIntTree (Node val children) = fromIntegral val + sumOfChildren
                               where sumOfChildren = (foldl (+) 0 $ map sumIntTree children)