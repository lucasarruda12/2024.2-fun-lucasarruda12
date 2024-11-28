copairing :: (a -> c) -> (b -> c) -> Either a b -> c
copairing f g e = case e of
  Left a -> f a
  Right b -> g b

-- manualmente
mplus :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mplus f g (Left a) = Left (f a)
mplus f g (Right b) = Right (g b)

-- Aproveitando a copairing
plus :: (a -> c) -> (b -> d) -> Either a b -> Either c d
plus f g = copairing (Left . f) (Right . g)
