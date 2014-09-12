-- Problem 41
import Euler
import Data.List
import Data.Maybe

orderedPermutations :: (Eq a) => [a] -> [[a]]
orderedPermutations [] = [[]]
orderedPermutations a = concatMap f a
  where f n = map (n:) . orderedPermutations $ delete n a

answer = fromMaybe 0 . find isPrime . map readDecimal
  $ orderedPermutations "7654321"

main = print answer
