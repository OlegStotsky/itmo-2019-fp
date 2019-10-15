import Control.DeepSeq

last'' :: (NFData a) => [a] -> a
last'' [] = error "empty list"
last'' [a] = a
last'' (x:xs) = deepseq x last'' xs