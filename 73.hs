-- Problem 73
import Data.Ratio

answer :: Int
answer = length [n%d | d <- [1..12000], 
                       n <- [(d `div` 3)..(d `div` 2)], 
                       gcd n d == 1, 
                       1%3 < n%d, 
                       n%d < 1%2]

main = print answer
