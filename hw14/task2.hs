{-# LANGUAGE RankNTypes #-}

import Data.Functor.Compose
import Control.Monad

joinWithDistrib :: (Monad m, Monad n) => (forall b. n (m b) -> m (n b)) -> m (n (m (n a))) -> m (n a)
joinWithDistrib f x = do a <- x
                         b <- f a
                         return (join b)

instance (Monad n, Monad m, Traversable n) => Monad (Compose m n) where
  return = \x -> Compose $ return $ return x
  (Compose mna) >>= g = Compose $ joinWithDistrib sequence (fmap (\na -> fmap (\a -> getCompose $ g a) na) mna)