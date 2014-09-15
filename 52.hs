-- Problem 52
import Euler
import Data.List
import Data.Function

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation s t = case s of
  [] ->  null t
  x:xs -> (x `elem` t) && isPermutation xs (delete x t)

hasPermutedMultiples :: Integer -> Bool
hasPermutedMultiples n = all ((isPermutation `on` decimal) n) multiples
  where multiples = take 6 $ iterate (+n) n

answer = head $ filter hasPermutedMultiples [1..]

main = print answer
