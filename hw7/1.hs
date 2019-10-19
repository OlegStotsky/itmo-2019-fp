import Data.List (foldl')

class Prolonged a where
  len :: a -> Integer

instance Prolonged [a] where
  len = foldl' (\acc val -> acc + 1) 0

instance Prolonged Integer where
  len x = len $ show x