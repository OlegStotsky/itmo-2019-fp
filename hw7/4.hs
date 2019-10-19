incrementBy :: (Enum a, Enum b) => a -> b -> b -> a
incrementBy base from to = _incrementBy base from to $ length $ enumFromTo from to
                            where _incrementBy base from to 0 = pred base
                                  _incrementBy base from to n = _incrementBy (succ base) from to (n-1) 