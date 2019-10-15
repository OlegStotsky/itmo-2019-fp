scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f acc [] = [acc]
scanl' f acc (x:xs) = seq acc (acc : scanl' f (f acc x) xs)