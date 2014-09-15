module Euler (
  base, binary, decimal, readBase, readBinary, readDecimal, numDigits,
  primes, isPrime, primeFactors, numUniquePrimeFactors,
  intSqrt, isPerfectSquare, triangleNums, isTriangleNum,
  pentagonalNums, isPentagonal, hexagonalNums,
  isPalindrome,
  isPandigital, is19Pandigital, is09Pandigital,
  mostCommon, hasDuplicates,
  alphabetPosition, wordValue,
  divides,
  (<==>), (</=>), (<&&>), (<||>)
) where

import MillerRabin
import Numeric
import Data.Char
import Data.List
import Data.List.Ordered
import Data.Ord
import Control.Applicative

base :: (Integral b, Integral a, Show a) => b -> a -> String
base i n = showIntAtBase (fromIntegral i) intToDigit n ""

binary :: (Integral a, Show a) => a -> String
binary = base 2

decimal :: (Integral a, Show a) => a -> String
decimal = base 10

readBase :: (Integral a, Integral b) => a -> String -> b
readBase i = fromIntegral . fst . head . readInt i isHexDigit digitToInt

readBinary :: (Integral a) => String -> a
readBinary = readBase 2

readDecimal :: (Integral a) => String -> a
readDecimal = readBase 10

numDigits :: (Integral a, Show a, Integral b) => a -> b
numDigits = fromIntegral . length . decimal

primes :: Integral a => [a]
primes = takeWhile (< 100) (sieve [2..]) ++ filter isPrime [100..]
  where sieve [] = []
        sieve (x:xs) = x : sieve [n | n <- xs, n `mod` x > 0]

isPrime :: Integral a => a -> Bool
isPrime n
  | n < primes !! 10 = n `member` primes
  | otherwise        = all (millerRabinPrimality m) $ take 10 primes
  where m = toInteger n

primeFactors :: Integral a => a -> [a]
primeFactors 1 = []
primeFactors n = p : primeFactors (n `div` p)
  where p = head $ filter (`divides` n) primes

numUniquePrimeFactors :: Integer -> Int
numUniquePrimeFactors = length . Data.List.nub . primeFactors

intSqrt :: Integral a => a -> a
intSqrt n = round $ sqrt x
  where x = fromIntegral n :: Double

isPerfectSquare :: Integral a => a -> Bool
isPerfectSquare n = n == intSqrt n ^ 2

triangleNums :: Integral a => [a]
triangleNums = map (\n -> n*(n+1) `div` 2) [0..]

isTriangleNum :: Integral a => a -> Bool
isTriangleNum n = n `member` triangleNums

pentagonalNums :: Integral a => [a]
pentagonalNums = map (\n -> n*(3*n-1) `div` 2) [0..]

hexagonalNums :: Integral a => [a]
hexagonalNums = map (\n -> n*(2*n-1)) [0..]

isPentagonal :: Integral a => a -> Bool
isPentagonal n = isPerfectSquare m && intSqrt m `mod` 6 == 5
  where m = 24*n+1

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll [] _ = True
containsAll (x:xs) l = x `elem` l && containsAll xs l

isPandigital :: (Integral a, Show a) => a -> Bool
isPandigital n = d <= 10 && containsAll (take d "1234567890") (decimal n)
  where d = numDigits n

is19Pandigital :: (Integral a, Show a) => a -> Bool
is19Pandigital = (>= 10^8) <&&> (< 10^9) <&&> isPandigital

is09Pandigital :: (Integral a, Show a) => a -> Bool
is09Pandigital = (>= 10^9) <&&> isPandigital

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (comparing length) . group . sort

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates = length </=> (length . Data.List.nub)

alphabetPosition :: Integral a => Char -> a
alphabetPosition c = fromIntegral $ (ord . toLower $ c) - ord 'a' + 1

wordValue :: Integral a => String -> a
wordValue = sum . map alphabetPosition

divides :: Integral a => a -> a -> Bool
divides m n = n `mod` m == 0

(<==>) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(<==>) = liftA2 (==)

(</=>) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(</=>) = liftA2 (/=)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
