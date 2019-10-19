import Data.List (foldl')

newtype ReaderList e a = ReaderList [(e -> a)]

instance Functor (ReaderList a) where
  fmap f (ReaderList xs) = ReaderList (map (f .) xs)
