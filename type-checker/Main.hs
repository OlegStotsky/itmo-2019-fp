import Control.Monad.State
import Control.Applicative
import qualified Data.Map

type Symb = String 
infixl 2 :@
infix 1 `alphaEq`

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)


unique :: Eq a => [a] -> [a]
unique [x] = [x]
unique [] = []
unique (x:xs) = let l = unique xs in if elem x l then l else x:l
          
freeVars :: Expr -> [Symb] 
freeVars (Var x) = [x]
freeVars (left :@ right) = unique $ (freeVars left) ++ (freeVars right)
freeVars (Lam x m) = Prelude.filter (/=x) (freeVars m)

subst :: Symb -> Expr -> Expr -> Expr
subst targetVar target source = evalState (substHelper targetVar target source) 0

substHelper :: Symb -> Expr -> Expr -> State Integer Expr
substHelper targetVar target r@(Var y) = if y == targetVar then return target else return r
substHelper targetVar target (l :@ r) = do l <- (substHelper targetVar target l)
                                           r <- (substHelper targetVar target r)
                                           return $ l :@ r
substHelper targetVar target l@(Lam x body) | x == targetVar = return l
substHelper targetVar target (Lam x body) 
                        | elem x (freeVars target) = do num <- get
                                                        let newName = "______" ++ (show num) ++ x
                                                        put $ num + 1
                                                        newLam <- substHelper x (Var newName) body 
                                                        substHelper targetVar target (Lam newName newLam)
substHelper targetVar target (Lam x body) = do newBody <- substHelper targetVar target body
                                               return $ Lam x newBody

alphaEq :: Expr -> Expr -> Bool
alphaEq left right = alphaEqHelper leftDeBrujin rightDeBrujin where
  alphaEqHelper left right = left == right
  leftDeBrujin = termToDeBruijn left
  rightDeBrujin = termToDeBruijn right

termToDeBruijn :: Expr -> Expr
termToDeBruijn exp = evalState (termToDeBruijinHelper exp Data.Map.empty) 0 where
  termToDeBruijinHelper :: Expr -> Data.Map.Map String Integer -> State Integer Expr
  termToDeBruijinHelper l@(Var x) m = do st <- get
                                         if Data.Map.member x m then return $ Var (show $ st - (m Data.Map.! x)) 
                                         else return l
  termToDeBruijinHelper (Lam x body) m = do st <- get
                                            put $ (st + 1)
                                            let newM = Data.Map.insert x st m
                                            newBody <- termToDeBruijinHelper body newM
                                            return $ Lam "" newBody
  termToDeBruijinHelper (l :@ r) m = do left <- (termToDeBruijinHelper l m)
                                        right <- (termToDeBruijinHelper r m)
                                        return $ left :@ right


reduceOnce :: Expr -> Maybe Expr
reduceOnce l@(Var x) = Nothing
reduceOnce (l@(Lam x body) :@ r) = Just $ subst x r body
reduceOnce (a :@ b) = case reduceOnce a of
                        Nothing -> case reduceOnce b of 
                          Nothing -> Nothing
                          Just exp -> Just $ a :@ exp
                        Just exp -> Just $ exp :@ b
reduceOnce (Lam x body) = do res <- reduceOnce body
                             return $ Lam x res

nf :: Expr -> Expr 
nf x = case reduceOnce x of
        Just newForm -> nf newForm
        Nothing -> x

infix 1 `betaEq`

betaEq :: Expr -> Expr -> Bool 
betaEq left right = let nfLeft = nf left; nfRight = nf right in
                      alphaEq nfLeft nfRight