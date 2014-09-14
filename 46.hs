-- Problem 46
import Euler
import Data.List.Ordered

sieve :: [Integer] -> [Integer]
sieve (x:xs)
  | isPrime x = sieve xs `minus` [x+2*n*n | n<-[1..]]
  | otherwise = x : sieve xs

answer = head . sieve $ 2:[3,5..]

main = print answer
