-- Problem 37
import Euler
import Data.List.Ordered

data LeftRight = L | R

-- All truncatable primes with a given direction and # of digits
truncatables :: LeftRight -> Integer -> [String]
truncatables _ 1 = ["2","3","5","7"]
truncatables lr n = filter (isPrime . readDecimal) $ case lr of
  L -> [d:ds | d <- "123456789", ds <- truncatables L (n-1),
               head ds `notElem` "24568"]
  R -> [ds ++ [d] | ds <- truncatables R (n-1), d <- "1379"]

-- All primes with a given direction
allTruncatables :: LeftRight -> [Integer]
allTruncatables lr = map readDecimal $ concatMap (truncatables lr) [2..]

-- All primes truncatable in both directions
allBothTruncatables :: [Integer]
allBothTruncatables = (allTruncatables L) `isect` (allTruncatables R)

answer = sum $ take 11 allBothTruncatables

main = print answer
