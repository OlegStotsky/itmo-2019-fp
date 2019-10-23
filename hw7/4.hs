incrementBy :: (Enum a, Enum b) => a -> b -> b -> a
incrementBy base from to = helper base from to $ length $ enumFromTo from to
                            where helper base from to 1 = base
                                  helper base from to 0 = base
                                  helper base from to n = helper (succ base) from to (n-1) 