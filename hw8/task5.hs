sumOrLen :: Either (Int,Int) String -> Int
sumOrLen = either (uncurry (+)) length