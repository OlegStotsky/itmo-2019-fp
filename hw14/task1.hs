{-# LANGUAGE RankNTypes #-}

import Control.Monad

joinWithDistrib :: (Monad m, Monad n) => (forall b. n (m b) -> m (n b)) -> m (n (m (n a))) -> m (n a)
joinWithDistrib f x = do a <- x
                         b <- f a
                         return (join b)