data Tree a = Node { value :: a
                   , children :: [Tree a] }

instance Functor Tree where
  fmap f (Node val children) = Node (f val) (map (fmap f) children)

height :: Tree a -> Int
height (Node _ children) = 1 + (foldl max 0 (map height children))

count :: Tree a -> Int
count (Node _ children) = 1 + (sum $ map count children)

min' :: Tree a -> a
min' (Node val []) = val
min' (Node _ (x:xs)) = min' x