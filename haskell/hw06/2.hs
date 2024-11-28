pairing :: (c -> a) -> (c -> b) -> c -> (a, b)
pairing f g c = (f c, g c)

-- Manualmente

mcross :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mcross f g (a, b) = (f a, g b)

-- Aproveitando a pairing
cross :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
cross f g = pairing (f . fst) (g . snd)
