module ExArith where

data ArEx = Atom Integer
          | Plus ArEx ArEx
          | Times ArEx ArEx
          | Neg ArEx
  deriving (Eq, Show)

-- pretty printer
pretty :: ArEx -> String
pretty (Atom x) = show x
pretty (Neg s) = "- " ++ pretty s
pretty (Plus s t) = "(" ++ pretty s ++ " + " ++ pretty t ++ ")"
pretty (Times s t) = "(" ++ pretty s ++ " * " ++ pretty t ++ ")"

-- example expressions
ex1 = (Atom 23) `Plus` (Atom 2)
ex2 = (Atom 7) `Times` ((Atom 7) `Plus` ((Atom 2) `Times` (Atom 8)))
ex3 = Times ex1 ex2
ex4 = Neg $ ex3 `Plus` ex1
ex5 = (Neg ex1) `Times` (Neg ex4)

-- eval evaluates an expression and returns its value
eval :: ArEx -> Integer
eval (Atom x)     = x
eval (Plus s t)   = (eval s) + (eval t)
eval (Times s t)  = (eval s) * (eval t)
eval (Neg s)    = - (eval s)

-- step should make only 1 step of calculation on a given ArEx
step :: ArEx -> ArEx
step (Plus s t) = case (s, t) of
                  (Atom x, Atom y) -> Atom (x + y)
                  (Atom _, t     ) -> Plus s (step t)
                  (s     , _     ) -> Plus (step s) t
step (Times s t) = case (s, t) of
                  (Atom x, Atom y) -> Atom (x * y)
                  (Atom _, t     ) -> Times s (step t)
                  (s     , _     ) -> Times (step s) t
step (Neg (Atom x)) = Atom (-x)
step (Neg s)        = step s
