import ExNat

data Int = MkInt Nat Nat
  deriving (Show)

instance Eq Int where
  (MkInt n n') == (MkInt m m') = n + m' == n' + m
