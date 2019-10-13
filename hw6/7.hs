data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

insert :: (Ord a) => a -> Tree a -> Tree a
insert a Leaf = Node a Leaf Leaf
insert a (Node x y z) = if a <= x then Node x (insert a y) z else Node x y (insert a z)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem a Leaf = False
treeElem a (Node x y z) = if a == x then True else treeElem a y || treeElem a z

delete :: (Ord a) => a -> Tree a -> Maybe (Tree a)
delete a Leaf = Nothing
-- 5
--2  4
delete a (Node x Leaf Leaf) = if a == x then Just Leaf else Nothing
-- 3
--2  5
--  4 
delete a (Node x left@(Node y y' y'') Leaf) = if a == x then Just left else case delete a left of 
                                                                                  Just leftTree -> Just (Node x leftTree Leaf)
                                                                                  Nothing -> Nothing
-- 3
--2  5
--    6
delete a (Node x Leaf right@(Node y y' y'')) = if a == x then Just right else case delete a right of
                                                                                    Just rightTree -> Just (Node x Leaf rightTree)
                                                                                    Nothing -> Nothing
-- 3
--2  5
--  4  6
delete a (Node x left@(Node y y' y'') right@(Node z z' z'')) = if a == x then case compare y z of
                                                                  LT -> Just (insert y right)
                                                                  EQ -> Just (insert y right)
                                                                  GT -> Just (insert z left)
                                                               else case delete a left of 
                                                                  Just leftTree -> Just (Node x leftTree right)
                                                                  Nothing -> case delete a right of
                                                                                Just rightTree -> Just (Node x left rightTree)
                                                                                Nothing -> Nothing