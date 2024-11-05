-- https://www.codewars.com/kata/5432d1c4913a65b67d00008d/train/haskell

newtype Hughes a = Hughes ([a] -> [a])

runHughes :: Hughes a -> [a]
runHughes (Hughes k) = k []

mkHughes :: [a] -> Hughes a
mkHughes = Hughes . (++)

------------------------------------------------------------

consDumb :: a -> Hughes a -> Hughes a
consDumb a h = mkHughes (a : runHughes h)

cons :: a -> Hughes a -> Hughes a
cons a (Hughes k) = Hughes $ (:) a . k

------------------------------------------------------------

appendDumb :: Hughes a -> Hughes a -> Hughes a
appendDumb a b = mkHughes (runHughes a ++ runHughes b)

instance Semigroup (Hughes a) where
  (<>) = error "todo"

instance Monoid (Hughes a) where
  mempty = error "todo"

------------------------------------------------------------