data Nat = Z | S Nat

foldNat :: (b -> b) -> b -> Nat -> b
foldNat f ini Z = ini
foldNat f ini (S x) = f $ foldNat f ini x

unfoldNat :: (a -> Maybe a) -> a -> Nat 
unfoldNat f a = case f a of 
                  Nothing -> Z
                  Just a -> S (unfoldNat f a)

pred' :: Nat -> Nat
pred' Z = Z
pred' x = fst $ foldNat (\p -> (snd p, S $ snd p)) (Z, Z) x

instance Num Nat where
  a + b = foldNat S a b
  a - b = foldNat pred' a b
  a * b = foldNat (\x -> x + x) a b
  fromInteger x = unfoldNat (\x -> if x == 0 then Nothing else Just $ x-1) x

instance Show Nat where
  show x = show $ foldNat succ 0 x

instance Enum Nat where
  toEnum = fromInteger . toInteger
  fromEnum x = foldNat (+1) 0 x