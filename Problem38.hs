module Problem38 (answer) where
import Euler
import Data.List

pandigitalMultiples :: Integer -> [Integer]
pandigitalMultiples n = filter isPandigital
  . map (readDecimal . concatMap (decimal . (*n))) $ drop 2 $ inits [1..9]

answer :: Integer
answer = maximum . concatMap pandigitalMultiples $ [1..9999]
