triples :: [(Integer, Integer, Integer)]
triples = do x <- [1..]
             y <- [1..x]
             z <- [1..y]
             if x^2 == y^2 + z^2 then return (x, y, z) else []