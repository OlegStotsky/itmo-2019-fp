data Tree a = Leaf | Node a (Tree a) (Tree a)

instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node x xs ys) = Node (f x) (f <$> xs) (f <$> ys)

instance Foldable Tree where
  foldr f ini Leaf = ini
  foldr f ini (Node x xs ys) = f x (foldr f (foldr f ini xs) ys)

instance Traversable Tree where
  traverse f Leaf = pure Leaf
  traverse f (Node x xs' ys') = pure Node <*> f x <*> traverse f xs' <*> traverse f ys'