-- Problem 50
import Euler
import Data.List

sublistsOfLength :: Int -> [a] -> [[a]]
sublistsOfLength n = takeWhile ((== n) . length) . map (take n) . tails

sublists :: [a] -> [[a]]
sublists xs = concat [sublistsOfLength m xs | m<-[n,n-1..0]]
  where n = length xs

answer = head . filter ((< 1000000) <&&> isPrime) . map sum . sublists
  $ takeWhile (< 5000) primes

main = print answer
