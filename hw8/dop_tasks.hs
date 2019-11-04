sumOfSquares :: [Integer] -> Integer
sumOfSquares = foldr (\val acc -> acc + val^2) 0

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\val acc -> if p val then val : acc else acc) []

zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys = reverse $ fst $ foldl (\(result, (y:ys)) val -> ((val, y):result, ys)) ([], ys) xs