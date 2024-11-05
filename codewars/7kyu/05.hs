-- https://www.codewars.com/kata/619f200fd0ff91000eaf4a08/train/haskell

data Parity = EITHER | EVEN | ODD

oddOrEven :: Word -> Parity
oddOrEven n | odd n          = EITHER
            | n `mod` 4 == 0 = EVEN
            | otherwise      = ODD
