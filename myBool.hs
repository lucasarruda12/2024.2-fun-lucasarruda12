import Prelude hiding ( Bool )

data Bool = True | False
    deriving ( Eq, Show )

data myThen a = then a
    deriving ( Eq, Show )

data Else a = else a
    deriving ( Eq, Show )

If :: Bool -> Then a -> Else a -> a 
If True (then a) _        = a
If False _       (else a) = a
