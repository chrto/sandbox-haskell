-- https://www.codewars.com/kata/53d8aea2e584dd9e67000ae6/train/haskell

data Solution = None | Single Double | Two Double Double
    deriving (Show)

solveQuadratics :: Double -> Double -> Double -> Solution
solveQuadratics 0 0 _ = None
solveQuadratics 0 b c = Single $ -c / b
solveQuadratics a 0 c
  | c == 0         = Single $ (-b) / (2 * a)
  | c > 0 && a > 0 = None
  | c < 0 && a < 0 = None
  | otherwise      = Two ((-b - e) / (2 * a)) ((-b + e) / (2 * a))
 where
  b = 0
  d = b ^ 2 - 4 * a * c
  e = sqrt d

solveQuadratics a b c
  | c == 0    = Two (b / a) 0
  | otherwise = Two ((-b - e) / (2 * a)) ((-b + e) / (2 * a))
 where
  d = b ^ 2 - 4 * a * c
  e = sqrt d
-- solveQuadratics a b c
--   | c == 0 = Two (b / a) 0
--   | otherwise = Two (firstPart + secondPart) (firstPart - secondPart)
--  where
--   firstPart  = -(b / (2 * a))
--   secondPart = sqrt (b ^ 2 - 4 * a * c) / 2 * a




-- solveQuadratics a b c
--   | d < 0     = None
--   | d == 0    = Single $ (-b) / (2 * a)
--   | otherwise = Two ((-b - e) / (2 * a)) ((-b + e) / (2 * a))
--  where
--   d = b ^ 2 - 4 * a * c
--   e = sqrt d



-- solveQuadratics :: Double -> Double -> Double -> Solution
-- solveQuadratics 0 0 _ = None
-- solveQuadratics 0 b c = Single $ -c / b
-- solveQuadratics a b c
--   | d < 0     = None
--   | d == 0    = Single $ (-b) / (2 * a)
--   | otherwise = let e = sqrt d in Two ((-b - e) / (2 * a)) ((-b + e) / (2 * a))
--   where d = b ^ 2 - 4 * a * c




      --   | otherwise = Two (l + k) (l - k)
    -- otherwise = let k = sqrt h / (2 * a) in Two (l + k) (l - k)
        --   l = (-b) / (2 * a)
        --   k = sqrt h / (2 * a)

-- solveQuadratics a 0 c | c == 0         = Single (-b / (2 * a))
--                       | c > 0 && a > 0 = None
--                       | c < 0 && a < 0 = None
--                       | otherwise      = Two sqrtx sqrtx
--  where
--   b     = 0
--   x     = c / 2 * a
--   sqrtx = sqrt x
-- solveQuadratics a b c
--   | b ^ 2 - 4 * a * c < 0 = None
--   | c == 0 = Two (b / a) 0
--   | otherwise = Two (firstPart + secondPart) (firstPart - secondPart)
--  where
--   firstPart  = -(b / (2 * a))
--   secondPart = sqrt (b ^ 2 - 4 * a * c) / 2 * a


-- solveQuadratics a b 0 = Two (b / a) 0
-- solveQuadratics a b c = Two (firstPart + secondPart) (firstPart - secondPart)
--  where
--   firstPart  = -(b / (2 * a))
--   secondPart = sqrt (b ^ 2 - 4 * a * c) / 2 * a

-- solveQuadratics a 0 c = Single $ -(sqrt (c / a))



-- solveQuadratics :: Double -> Double -> Double -> Solution
-- solveQuadratics a b c = let d = b ^ 2 - 4 * a * c in go a b c d
--  where
--   go a b c d
--     | a == 0 && b == 0
--     = None
--     | a == 0
--     = Single (-c / b)
--     | d < 0
--     = None
--     | d == 0
--     = Single (-b / (2 * a))
--     | otherwise
--     = let d2 = sqrt d in Two ((-b - d2) / (2 * a)) ((-b + d2) / (2 * a))



-- solveQuadratics :: Double -> Double -> Double -> Solution
-- solveQuadratics 0 0 _ = None
-- solveQuadratics 0 b c = Single $ -c / b
-- solveQuadratics a b c
--   | d < 0     = None
--   | d == 0    = Single $ (-b) / (2 * a)
--   | otherwise = let e = sqrt d in Two ((-b - e) / (2 * a)) ((-b + e) / (2 * a))
--   where d = b ^ 2 - 4 * a * c
