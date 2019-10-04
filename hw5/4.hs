applyMaybeList :: (a -> Maybe b) -> [a] -> Maybe [b]
applyMaybeList f [] = Just []
applyMaybeList f (x:xs) = case applyMaybeList f xs of 
                               Nothing -> Nothing
                               Just l -> case f x of 
                                              Nothing -> Nothing
                                              Just x -> Just (x : l)
