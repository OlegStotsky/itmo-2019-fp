type Church a = (a -> a) -> a -> a

prev :: Church a -> Church a
prev n = fst (n (\p -> pair (snd p) (plus one (snd p))) (pair zero zero))
    where 
      k x y = x
      k2 x y = y
      fst p = p k
      snd p = p k2
      pair x y f = f x y
      one s z = s z
      zero s z = z
      plus n1 n2 s z = n1 s (n2 s z)

even' :: (Integral a) => a -> Bool
even' x 
       | x == 0  = True
       | x == 1  = False
       | otherwise = even' (x-2)

oddPlus x y | (not $ even x) && (not $ even y) = x + y
            | otherwise = 0

infixl 6 +??? 
a +??? b | a > 0 = succ ((pred a) +??? b)
         | a < 0 = pred ((succ a) +??? b)
         | a == 0 = b

collatz :: Integer -> Integer
collatz n 
        | n == 1 = 1
        | even n = collatz (div n 2)
        | otherwise = collatz (3*n + 1)
      
myhof :: (a -> b) -> (a -> c) -> a -> (b, c)
myhof f g x = (f x, g x)

strangePlus :: Integer -> Integer -> Integer
strangePlus a b
    | b == 0 = a
    | b < 0 = pred $ strangePlus a (succ b)
    | otherwise = succ $ strangePlus a (pred b)
