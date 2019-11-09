import Data.Bool

sumOfSquares :: [Integer] -> Integer
sumOfSquares = foldr (\val acc -> acc + val^2) 0

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\val acc -> bool  acc (val : acc) (p val)) []

zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys = reverse $ fst $ foldl (\(result, (y:ys)) val -> ((val, y):result, ys)) ([], ys) xs