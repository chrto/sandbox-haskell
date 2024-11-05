-- https://www.codewars.com/kata/555eded1ad94b00403000071/train/haskell
import           Data.List                      ( genericTake )
import           Numeric                        ( showFFloat )

seriesSum :: Integer -> String
seriesSum = formatFloat2 . sum . generateArray
 where
  generateArray :: Integer -> [Float]
  generateArray 0 = []
  generateArray n = 1 / fromIntegral (1 + 3 * (n - 1)) : generateArray (n - 1)

  formatFloat2 :: Float -> String
  formatFloat2 floatNum = showFFloat (Just 2) floatNum ""

seriesSum' :: Integer -> String
seriesSum' = formatFloat2 . sum . generateArray
 where
  generateArray :: Integer -> [Float]
  generateArray = flip genericTake $ (1 /) <$> [1, 4 ..]

  formatFloat2 :: Float -> String
  formatFloat2 floatNum = showFFloat (Just 2) floatNum ""
