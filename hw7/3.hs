data Useless a = Useless

instance Functor Useless where
  fmap f Useless = Useless