data Nat = Zero | Suc Nat deriving (Show)

foldNat :: (a -> a) -> a -> Nat -> a
foldNat _ acc (Zero) = acc
foldNat f acc (Suc x) = foldNat f (f acc) x

sumNat :: Nat -> Nat -> Nat
sumNat a b = foldNat suc b a
           where suc x = Suc x