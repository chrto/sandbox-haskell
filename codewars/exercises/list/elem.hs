import           Prelude                 hiding ( elem )

elem :: (Eq a) => a -> [a] -> Bool
elem e xs = not $ null $ [ x | x <- xs, x == e ]

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' e xs = or $ [ x == e | x <- xs ]

elem' _ []       = False
elem' e (x : xs) = x == e || elem' e xs
