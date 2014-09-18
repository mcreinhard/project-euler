-- Problem 53
import Euler

choose :: Integer -> Integer -> Integer
choose n k
  | n == 0 || k == 0 = 1
  | otherwise        = (n * choose (n-1) (k-1)) `div` k

answer = length $ filter (> 10^6) combinations
  where combinations = [n `choose` k | n<-[1..100], k<-[1..n]]

main = print answer
