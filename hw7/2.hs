{-# LANGUAGE RankNTypes #-}

newtype Church a = Church (forall a. (a -> a) -> a -> a)

zero :: Church a
zero = Church $ \s z -> z

one :: Church a
one = Church $ \s z -> s z

suc (Church a) = Church (\s z -> s (a s z))

prev (Church n) = Church $ fst (n (\p -> pair (snd p) (plus one (snd p))) (pair zero zero))
    where 
      k x y = x
      k2 x y = y
      fst p = p k
      snd p = p k2
      pair x y f = f x y
      one s z = s z
      zero s z = z
      plus n1 n2 s z = n1 s (n2 s z)

instance Num (Church a) where
  (Church a) + (Church b) = Church (\s z -> a s (b s z))
  (Church a) * (Church b) = Church (\s z -> a (b s) z)
  first@(Church a) - second@(Church b) = b prev first
  fromInteger x = helper x zero
                   where helper 0 n = n
                         helper x n = helper (x-1) (suc n)