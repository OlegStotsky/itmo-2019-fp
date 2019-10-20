f :: (a -> b) -> Maybe (e, [Maybe a]) -> Maybe (e, [Maybe b])
f g = fmap (\tuple -> fmap (\list -> fmap (\mb -> fmap g mb) list) tuple)

f' :: (a -> b) -> Maybe (e, [Maybe a]) -> Maybe (e, [Maybe b])
f' = fmap . fmap . fmap . fmap

g :: (a -> b) -> Maybe (e, [[Maybe a]]) -> Maybe (e, [[Maybe b]])
g = fmap . fmap . fmap . fmap . fmap