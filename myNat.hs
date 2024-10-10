import Prelude hiding ( Num(..), min, max, quot, pred )

data Nat = O | S Nat
    deriving ( Show, Eq )

(+) :: Nat -> Nat -> Nat
n + O     = n
n + (S m) = S (n + m)

(*) :: Nat -> Nat -> Nat
n * O     = O
n * (S m) = n + (n * m)

double :: Nat -> Nat
double = (*) ( S $ S $ O )

pred :: Nat -> Nat
pred O     = O
pred (S n) = n

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
