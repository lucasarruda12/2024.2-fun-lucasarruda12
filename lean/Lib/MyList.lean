import Lib.MyNat

namespace MyList

inductive List (α : Type) where
  | nil : List α
  | cons : α → List α → List α

infixr:50 " :: " => List.cons

open List
open MyNat

-- Definitions
open MyNat
open MyNat.Nat

def take : MyNat.Nat → List α → List α
  | S n, x :: xs => x :: (take n xs)
  | _, _ => nil

def drop : MyNat.Nat → List α → List α
  | S n, _ :: xs => (drop n xs)
  | _, l => l

def concat : List α → List α → List α
  | nil, ys => ys
  | x :: xs, ys => x :: (concat xs ys)

infixr:60 " ++ " => concat

-- Theorems
variable (n m : MyNat.Nat)
variable (xs ys : List α)

theorem take_concat_drop : take n xs ++ drop n xs = xs := by
  revert n
  induction xs with
  | nil =>
    intro n
    calc
      take n nil ++ drop n nil
        = nil ++ nil := by rw [take, drop];simp;simp
      _ = nil        := by rw [concat]

  | cons x xs hi =>
    intro n
    cases n with
    | O =>
      calc
        take O (x :: xs) ++ drop O (x :: xs)
          = nil ++ (x :: xs) := by rw [take, drop];simp;simp
        _ = (x :: xs)        := by rw [concat]

    | S n =>
      calc
        take (S n) (x :: xs) ++ drop (S n) (x :: xs)
          = (x :: (take n xs)) ++ (drop n xs)   := by rw [take, drop]
        _ = (x :: ((take n xs) ++ (drop n xs))) := by rw [concat]
        _ = (x :: xs)                           := by rw [hi]

-- This is both the first and the last time
-- I'm writting proofs like this.
-- Very readable, very pretty.. PITA to write!

end MyList
