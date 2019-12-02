newtype MyFn e a = MyFn { myFn :: e -> a }

instance Functor (MyFn e) where
  fmap g (MyFn f) = MyFn $ fmap g f

instance Applicative (MyFn e) where
  pure x = MyFn $ \_ -> x
  MyFn g <*> MyFn f = MyFn $ \x -> g x (f x)

instance Monad (MyFn e) where
  MyFn f >>= g = MyFn $ \x -> myFn (g (f x)) x