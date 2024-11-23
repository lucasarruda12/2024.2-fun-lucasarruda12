namespace MyBool

-- Definitions
inductive Bool where
  | T : Bool
  | F : Bool

open Bool

def bnot : Bool → Bool
  | T => F
  | F => T


-- Theorems
theorem bnot_involution : bnot ∘ bnot = id := by
  funext b
  rw [Function.comp, id]
  cases b with
  | F => rw [bnot, bnot]
  | T  => rw [bnot, bnot]

end MyBool
