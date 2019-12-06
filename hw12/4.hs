import Control.Monad.State

askState :: State r r
askState = state $ \s -> (s, s)

asksState :: (r -> a) -> State r a
asksState f = do
  env <- get
  return $ f env

localState :: (r -> r) -> State r a -> State r a
localState f st = do
  oldEnv <- get
  put $ f oldEnv
  val <- st
  put oldEnv
  return val

readerState :: (r -> a) -> State r a
readerState f = state $ \s -> (f s, s)