-- Problem 49
import Euler
import Data.List
import Data.Function
import Data.Maybe

-- All prime permutations of a number
primePermutations :: Integer -> [Integer]
primePermutations = filter isPrime . map readDecimal . permutations . decimal

-- All pairs of numbers in ns, with their difference
differences :: [Integer] -> [((Integer, Integer), Integer)]
differences ns = [((m,n),n-m) | m<-ns, n<-ns, m < n]

-- Equidistant pairs of prime permutations, grouped by distance
equidistantPermutations :: Integer -> [[(Integer, Integer)]]
equidistantPermutations = map (map fst) . groupBy ((==) `on` snd)
  . sortBy (compare `on` snd) . differences . primePermutations

-- All 3-element arithmetic sequences, from a list of equidistant pairs
arithmeticSequences :: [(Integer, Integer)] -> [[Integer]]
arithmeticSequences pairs =
  [[fst x, snd x, snd y] | x<-pairs, y<-pairs, snd x == fst y]

-- The prime permutation sequence containing a number, if one exists
primePermSequence :: Integer -> Maybe [Integer]
primePermSequence =
  listToMaybe . concatMap arithmeticSequences . equidistantPermutations

answer = readDecimal $ concatMap decimal answerSequence
  where answerSequence = fromJust . find (((>= 1000) <&&> (/= 1487)) . head)
          $ mapMaybe primePermSequence [1000..9999]

main = print answer
