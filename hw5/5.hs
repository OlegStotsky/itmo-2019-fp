isFib :: Integer -> Bool
isFib x = computeFibs 0 1
        where computeFibs :: Integer -> Integer -> Bool
              computeFibs fi se = case compare se x of
                                          LT -> computeFibs se (fi + se)
                                          EQ -> True
                                          GT -> False

countFibs :: [Integer] -> Integer
countFibs = fromIntegral . length . filter isFib