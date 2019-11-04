foldEither :: (a -> b) -> (c -> b) -> Either a c -> b
foldEither f _ (Left x) = f x
foldEither _ g (Right x) = g x

foldPair :: (a -> b -> c) -> (a, b) -> c
foldPair f (x, y) = f x y

sumOrLen :: Either (Int,Int) String -> Int
sumOrLen = foldEither (foldPair (+)) length