import Prelude hiding ( Num(..), min, max, quot )

data Nat = O | S Nat
    deriving ( Show, Eq )

(+) :: Nat -> Nat -> Nat
n + O     = n
n + (S m) = S (n + m)

(*) :: Nat -> Nat -> Nat
n * O     = O
n * (S m) = n + (n * m)

fact :: Nat -> Nat
fact O = S O
fact (S n) = (S n) * (fact n)

fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = fib (S n) + fib n

min :: (Nat, Nat) -> Nat
min (n, O)         = O
min (O, n)         = O
min ((S n), (S m)) = S (min (n, m))

max :: (Nat, Nat) -> Nat
max (n, O)         = n
max (O, n)         = n
max ((S n), (S m)) = S (max (n, m))

-- monos :: Nat -> Nat -> Nat
-- monos O n = O
-- monos n O = n
-- monos (S n) (S m) = monos n m
-- 
-- (-) :: Nat -> Nat -> Nat
-- (-) = monos
-- 
-- quot :: (Nat, Nat) -> Nat
-- quot (_, O) = error "Division by O"
-- quot (n, m)
    -- | menor == n = O 
    -- | otherwise  = S (quot (n - m, m))
    -- where menor = min (n, m)
-- 
