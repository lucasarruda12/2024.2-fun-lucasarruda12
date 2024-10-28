{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O      = "O"
    show (S n)  = "S" ++ show n

instance Eq Nat where

    O     == O      = True
    (S n) == (S m)  = n == m
    _     == _      = False

instance Ord Nat where

    O     <= _      = True
    _     <= O      = False
    (S n) <= (S m)  = n <= m
    

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O _ = O
    min _ O = O
    min (S n) (S m) = S (min n m)

    max O n = n
    max n O = n
    max (S n) (S m) = S (min n m)


----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O      = O
pred (S n)  = n

even :: Nat -> Bool
even O          = True
even (S O)      = False
even (S (S n))  = even n

odd :: Nat -> Bool
odd O          = False
odd (S O)      = True
odd (S (S n))  = even n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O     = n
n <+> (S m) = S(n <+> m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
O <-> n          = O
n <-> O          = n
(S n) <-> (S m)  = n <-> m

-- multiplication
(<*>) :: Nat -> Nat -> Nat
n <*> O     = O
n <*> (S m) = n + (n <*> m)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
n <^> O     = S O
n <^> (S m) = n * (n <^> m)

-- quotient
(</>) :: Nat -> Nat -> Nat
n </> O = error "division by zero"
n </> m 
  | n == m    = S O
  | n <= m    = O
  | otherwise = n </> (n <-> m)

-- remainder
(<%>) :: Nat -> Nat -> Nat
n <%> O = error "division by zero"
n <%> m 
  | n == m    = O
  | n <= m    = n
  | otherwise = n </> (n <-> m)

-- divides
(<|>) :: Nat -> Nat -> Bool
n <|> m = isZero (n <%> m)

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff n m
  | n <= m    = m <-> n
  | otherwise = n <-> m

(|-|) = absDiff

factorial :: Nat -> Nat
factorial O     = S O
factorial (S n) = S n <*> factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = 0
sg _ = 1

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat x
  | x == 0    = O
  | x > 0     = S (toNat (x-1))
  | otherwise = toNat (- x)

fromNat :: Integral a => Nat -> a
fromNat O     = 0
fromNat (S n) = 1 + fromNat(n)

-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = fromInteger (- x)
      | x == 0    = O
      | otherwise = S (fromInteger (x - 1))

