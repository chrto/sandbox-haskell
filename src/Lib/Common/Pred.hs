module Lib.Common.Pred (Predicate, oneOf) where

type Predicate a = a -> Bool

instance Monoid (Predicate a) where
 mempty = False

oneOf :: [Predicate a] -> a -> Bool
oneOf [] _ = False
oneOf (p:ps) val = p val || oneOf ps val

oneOf' :: [Predicate a] -> Predicate a -> Bool
oneOf' ps p' v= foldr (\p -> p v) (\_ -> False) ps

