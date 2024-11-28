module ExArithV where

-- modify ExArith to allow for variables

-- decide how to represent Assignments:
type Assignment = ()

data ArExV = Atom Integer
           | Var Char
           | Plus ArExV ArExV
           | Times ArExV ArExV
           | Neg ArExV

-- pretty printer
pretty :: ArExV -> String
pretty (Atom x) = show x
pretty (Var c)  = show c
pretty (Neg s) = "- " ++ pretty s
pretty (Plus s t) = "(" ++ pretty s ++ " + " ++ pretty t ++ ")"
pretty (Times s t) = "(" ++ pretty s ++ " * " ++ pretty t ++ ")"
 
ex1 = (Var 'c') `Plus` (Atom 2)
ex2 = (Atom 7) `Times` ((Atom 7) `Plus` ((Atom 2) `Times` (Atom 8)))

-- eval evaluates an expression and returns its value
-- eval :: ?
eval = undefined

