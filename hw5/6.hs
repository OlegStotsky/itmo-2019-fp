data Arg = Arg String
data Term = Var String | Application Term Term | Abstraction Arg Term

isFreeIn :: String -> Term -> Bool
isFreeIn var (Var x) = var == x
isFreeIn var (Application x y) = isFreeIn var x || isFreeIn var y
isFreeIn var (Abstraction (Arg x) y) = not (var == x) && isFreeIn var y