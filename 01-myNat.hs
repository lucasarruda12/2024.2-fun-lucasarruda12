data Nat = O | S Nat
    deriving ( Show )

(+) :: Nat -> Nat -> Nat
n + O     = n
n + (S m) = S (n + m)

(.) :: Nat -> Nat -> Nat
n . O     = O
n . (S m) = n + (n . m)


