isFib :: Integer -> Bool
isFib x = computeFibs 0 1
        where computeFibs :: Integer -> Integer -> Bool
              computeFibs fi se = case compare se x of
                                          LT -> computeFibs se (fi + se)
                                          EQ -> True
                                          GT -> False

countsFibs :: [Integer] -> Integer
countsFibs l = foldl (\acc val -> if val then acc + 1 else acc) 0 (map isFib l)