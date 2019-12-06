import Control.Monad.State

writerState :: Monoid b => (a, b) -> State b a
writerState (a, b) = state $ \s -> (a, s <> b)

tellState :: Monoid a => a -> State a ()
tellState x = writerState ((), x)

listenState :: Monoid a => State a b -> State a (b, a)
listenState st = do
  x <- st
  s <- get
  return (x, s)

listensState :: Monoid a => (a -> w) -> State a b -> State a (b, w)
listensState f st = do
  x <- st
  s <- get
  return (x, f s)

censorState :: Monoid a => (a -> a) -> State a b -> State a b
censorState f st = do
  oldS <- get
  put mempty
  val <- st
  newS <- get
  put (oldS <> f newS)
  return val