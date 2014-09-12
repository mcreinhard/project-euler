-- Problem 38
import Euler
import Data.List

pandigitalMultiples :: Integer -> [Integer]
pandigitalMultiples n = filter is19Pandigital
  . map (readDecimal . concatMap (decimal . (*n))) $ drop 2 $ inits [1..9]

answer = maximum . concatMap pandigitalMultiples $ [1..9999]

main = print answer
