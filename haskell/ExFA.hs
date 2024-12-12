{-# LANGUAGE KindSignatures #-}

module FA where

import Prelude hiding
    ( Functor(..)
    , fmap
    , (<$>) , (<$) , ($>) , (<&>)
    , unzip
    , Applicative(..)
    , pure
    , (<*>) , (<*) , (*>) , (<**>)
    , liftA , liftA2 , liftA3
    )

import qualified Data.Functor as F
import qualified Control.Applicative as A


----------------------------------------------------------------
-- Functor
----------------------------------------------------------------

class Functor (f :: * -> *) where

  fmap :: (a -> b) -> (f a -> f b)

  (<$) :: b -> f a -> f b
  (<$) = \b -> fmap (\x -> b)

  {- LAWS

     fmap id = id
     fmap (f . g) = fmap f . fmap g

   -}

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

unzip :: Functor f => f (a, b) -> (f a, f b)
unzip = undefined 

void :: Functor f => f a -> f ()
void = fmap (\x -> ())

-- syntactic associativity and precedence
infixl 4 <$>, $>, <$
infixl 1 <&>

----  Instances  -----------------------------------------------

-- List
instance Functor [] where
    fmap f []       = []
    fmap f (x : xs) = f x : (fmap f xs)

-- Maybe
instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just a) = Just (f a)

-- (α ×)
pairing :: (d -> a) -> (d -> b) -> d -> (a, b)
pairing f g a = (f a, g a)

cross :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
cross f g = pairing (f . fst) (g . snd)

instance Functor ((,) a) where
    fmap = cross id

-- (α +)
copairing :: (a -> d) -> (b -> d) -> Either a b -> d
copairing f g (Left a) = f a
copairing f g (Right b) = g b

cocross :: (a -> c) -> (b -> d) -> Either a b -> Either c d
cocross f g = copairing (Left . f) (Right . g)

instance Functor (Either a) where
   fmap = cocross id

-- (r →)
instance Functor ((->) r) where
    fmap = (.)

-- IO
instance Functor IO where
    fmap f ioa = do 
      a <- ioa
      pure (f a)


----------------------------------------------------------------
-- Applicative
----------------------------------------------------------------

class Functor f => Applicative (f :: * -> *) where

  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

  {- LAWS

     pure id <*> v = ??
     pure f <*> pure x = ??
     u <*> pure y = ??
     u <*> (v <*> w) = ??

   -}

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f = (<*>) (pure f)

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f fa = (<*>) (pure f <*> fa)

-- sequence actions, discarding the value of the first argument
(*>) :: Applicative f => f a -> f b -> f b
(*>) = undefined

-- sequence actions, discarding the value of the second argument
(<*) :: Applicative f => f a -> f b -> f a
(<*) = undefined

-- A variant of (<*>) with the types of the arguments reversed.
-- It differs from flip (<*>) in that the effects are resolved
-- in the order the arguments are presented.
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = undefined

when :: Applicative f => Bool -> f () -> f ()
when = undefined

unless :: Applicative f => Bool -> f () -> f ()
unless = undefined

sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL = undefined

-- syntactic associativity and precedence
infixl 4 <*>, *>, <*, <**>

----  Instances  -----------------------------------------------

-- Maybe
instance Applicative Maybe where
    pure = undefined
    (<*>) = undefined

-- Lists with ambiguous computational aspect (non-determinism):
-- Create an isomorphic copy of the type List a, called Ambiguous a
newtype Ambiguous a = Ambiguous [a]

-- show ambiguous lists like this: ?[1,2,3]
instance Show a => Show (Ambiguous a) where
  show (Ambiguous xs) = "?" <> show xs

instance Functor Ambiguous where
    fmap = undefined

instance Applicative Ambiguous where
    pure = undefined
    (<*>) = undefined

-- Lists with temporal computational aspect (sequencial):
-- Create an isomorphic copy of the type List a, called Temporal a
-- (the isomorphism is given by the constructor/wrapper Temporal : List a -> Temporal a)
newtype Temporal a = Temporal [a]

-- show temporal lists like this: →[1,2,3]
instance Show a => Show (Temporal a) where
  show (Temporal xs) = "→" <> show xs

instance Functor Temporal where
    fmap = undefined

instance Applicative Temporal where
    pure = undefined
    (<*>) = undefined

-- IO
instance Applicative IO where
    pure = undefined
    (<*>) = undefined

-- (m ×)
instance Monoid m => Applicative ((,) m) where
    pure = undefined
    (<*>) = undefined

-- (s +)
instance Semigroup s => Applicative (Either s) where
    pure = undefined
    (<*>) = undefined

-- (r →)
instance Applicative ((->) r) where
    pure = undefined
    (<*>) = undefined

