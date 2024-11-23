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
import ExNat

data Int = Int Nat Nat
  deriving (Show)

instance Eq Int where
  (Int n n') == (Int m m') = n + m' == n' + m

