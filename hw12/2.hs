import Control.Monad.Reader

data Lam a = Var a | App (Lam a) (Lam a) | Lam a (Lam a)

hasFreeVars :: Eq a => Lam a -> Bool
hasFreeVars term = runReader (helper term) [] where
  helper :: Eq a => Lam a -> Reader [a] Bool
  helper (Var x) = asks (not . elem x)
  helper (App left right) = do 
    leftHasFreeVars <- helper left
    rightHasFreeVars <- helper right
    return $ leftHasFreeVars || rightHasFreeVars
  helper (Lam x term) = local (x:) (helper term)