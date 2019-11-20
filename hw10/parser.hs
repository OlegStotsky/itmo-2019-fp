import Data.Char

data Parser a = Parser { runParser :: String -> [(String, a)] }

eof :: Parser ()
eof = Parser f where
  f xs = if null xs then [(xs, ())] else []

char :: Char -> Parser Char
char c = Parser f where
  f (x:xs) | c == x = [(xs, x)]
  f _  = []

optional :: Parser a -> Parser (Maybe a)
optional f = Parser g where
  g xs = (xs, Nothing) : ((fmap . fmap) Just $ runParser f xs)

space :: Parser Char
space = Parser f where
  f (x:xs) | isSpace x = [(xs, x)]
  f _ = []

spaces :: Parser String
spaces = Parser f where
  f xs = helper xs "" where
    helper (x:xs) curStr | isSpace x = (xs, ' ' : curStr) : helper xs (' ' : curStr)
    helper _ _ = []

int :: Parser Int
int = Parser f where
  f (x:xs) | isDigit x = [(xs, (digitToInt x))]
  f _ = []

instance Functor Parser where
  fmap g p = Parser f where
    f xs = (fmap . fmap) g $ (runParser p xs)

instance Applicative Parser where
  pure x = Parser $ \xs -> [(xs, x)]
  Parser u <*> Parser v = Parser f where
    f xs = mconcat $ fmap (\(str, f) -> fmap (\x -> fmap f x) (v str)) $ u xs