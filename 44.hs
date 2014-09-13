-- Problem 44
import Euler

posPentagonalNums :: [Integer]
posPentagonalNums = tail pentagonalNums

pentagonalPairs :: [(Integer, Integer)]
pentagonalPairs = [(m,n) | n <- posPentagonalNums,
                           m <- takeWhile (< n) posPentagonalNums,
                           isPentagonal (m+n), isPentagonal (n-m)]

answer = n-m where (m,n) = head pentagonalPairs

main = print answer
