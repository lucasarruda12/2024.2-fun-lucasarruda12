namespace MyNat

inductive Nat where
  | O : Nat
  | S : Nat → Nat

open Nat

def add (n m : Nat) : Nat :=
  match m with
  | O   => n
  | S m => S (add n m)
infixr:50 " + " => add

def mul (n m : Nat) : Nat :=
  match m with
  | O => O
  | S m => add (mul n m) n

end MyNat
