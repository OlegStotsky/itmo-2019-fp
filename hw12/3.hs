import Data.Set
import Control.Monad.State

data Lam a = Var a | App (Lam a) (Lam a) | Lam a (Lam a) deriving Show


getFreeVars :: Ord a => Lam a -> Set a
getFreeVars x = snd $ runState (helper x) empty where
  helper :: Ord a => Lam a -> State (Set a) ()
  helper (Var a) = state $ \s -> ((), insert a empty)
  helper (App x y) = helper x >> helper y
  helper (Lam x y) = do
    helper y
    s <- get
    put $ delete x s