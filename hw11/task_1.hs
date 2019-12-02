term a b = do
  a
  b
  if 3 < 5
    then do
      a
      return 4
    else return 5
  x <- a
  let y = 5
  [x, y] <- b
  [x, y, z] <- a
  x <- x
  x
  pure x

term' a b = a >> b >>  (if 3 < 5 then a >> return 4 else return 5) >> a >>= (\x -> let y = 5 in b >>= let f [x, y] = a >>= (let g [x, y, z] = x >>= (\x -> x >> pure x); g _ = fail ""; in g); f _ = fail ""; in f)