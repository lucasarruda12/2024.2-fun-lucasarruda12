module ExArithV where

-- modify ExArith to allow for variables

-- decide how to represent Assignments:
type Assignment = ()

data ArExV = Atom Integer
           | Plus ArExV ArExV
           | Times ArExV ArExV
           | Neg ArExV

-- pretty printer
pretty :: ArExV -> String
pretty = undefined

-- eval evaluates an expression and returns its value
-- eval :: ?
eval = undefined

