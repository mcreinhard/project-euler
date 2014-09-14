-- Problem 47
import Euler
import Data.List

primeFactors :: Integral a => a -> [a]
primeFactors 1 = []
primeFactors n = p : primeFactors (n `div` p)
  where p = head $ filter (`divides` n) primes

numFactors :: Integer -> Int
numFactors = length . nub . primeFactors

answer = head $ filter (allHaveFourFactors . take 4 . iterate (+1)) [1..]
  where allHaveFourFactors = all $ (== 4) . numFactors

main = print answer
