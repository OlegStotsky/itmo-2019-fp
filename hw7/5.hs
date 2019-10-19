uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

cons' :: Maybe (a, [a]) -> [a]
cons' Nothing = []
cons' (Just (x, xs)) = x : xs