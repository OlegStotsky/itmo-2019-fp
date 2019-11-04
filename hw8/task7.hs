import Data.List

fibs :: [Integer]
fibs =  unfoldr (\(first, second) -> Just (first, (second, first + second))) (0, 1)