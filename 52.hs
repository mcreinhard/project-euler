-- Problem 52
import Euler
import Data.Function

hasPermutedMultiples :: Integer -> Bool
hasPermutedMultiples n = all ((isPermutation `on` decimal) n) multiples
  where multiples = take 6 $ iterate (+n) n

answer :: Integer
answer = head $ filter hasPermutedMultiples [1..]

main :: IO ()
main = print answer
