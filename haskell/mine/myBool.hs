import Prelude hiding ( Bool )

data Booleano = Verdadeiro | Falso
    deriving ( Eq, Show )

data Entao a = Entao a
    deriving ( Eq, Show )

data Senao a = Senao a
    deriving ( Eq, Show )

lucas_é_bonito = Verdadeiro

se :: Booleano -> Entao a -> Senao a -> a
se Verdadeiro (Entao a) _         = a
se Falso      _         (Senao b) = b
