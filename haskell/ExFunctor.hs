module ExFunctor where

import Prelude hiding ( fmap , (<$) )

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b        -> f a -> f b
  (<$) = fmap . const


instance Funktor [] where
    fmap = map

instance Funktor Maybe where
    fmap = mmap

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f Nothing  = Nothing
mmap f (Just a) = Just (f a)

-- what about Either?
instance Funktor (Either a) where
  fmap = emap

emap :: (b -> c) -> Either a b -> Either a c
emap f (Left a) = Left a
emap f (Right b) = Right (f b)

-- what about pairs?
instance Funktor ((,) a) where
  fmap = pmap

pmap :: (b -> c) -> (a, b) -> (a, c)
pmap f (a, b) = (a, f b)

-- what about functions?
instance Funktor ((->) a) where
  fmap = (.)

-- what about Trees?

-- ...define Functor instances of as many * -> * things as you can think of!

