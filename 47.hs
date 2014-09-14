-- Problem 47
import Euler
import Data.List

-- Note: to run in a reasonable amount of time, this must be compiled with -O2,
-- and the functions numUniquePrimeFactors and primeFactors (in Euler.hs) must
-- be in the same file.

answer = head $ filter (allHaveFourFactors . take 4 . iterate (+1)) [1..]
  where allHaveFourFactors = all $ (== 4) . numUniquePrimeFactors

main = print answer
