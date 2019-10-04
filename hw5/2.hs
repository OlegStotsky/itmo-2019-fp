data Nat = Zero | Suc Nat

foldNat :: (a -> a) -> a -> Nat -> a
foldNat _ acc (Zero) = acc
foldNat f acc (Suc x) = foldNat f (f acc) x