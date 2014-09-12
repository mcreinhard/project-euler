module Euler (
  base,
  binary,
  decimal,
  readBase,
  readBinary,
  readDecimal,
  numDigits,
  isPalindrome,
  primes,
  isPrime,
  isPandigital,
  is19Pandigital,
  is09Pandigital,
  triangleNums,
  isTriangleNum,
  alphabetPosition,
  wordValue,
  isPerfectSquare,
  intSqrt,
  mostCommon
) where

import MillerRabin
import Numeric
import Data.Char
import Data.List
import Data.List.Ordered
import Data.Ord
import Control.Applicative

base :: (Integral a, Show a) => a -> a -> String
base i n = showIntAtBase i intToDigit n ""

binary :: (Integral a, Show a) => a -> String
binary = base 2

decimal :: (Integral a, Show a) => a -> String
decimal = base 10

readBase :: (Integral a) => a -> String -> a
readBase i = fst . head . readInt i isHexDigit digitToInt

readBinary :: (Integral a) => String -> a
readBinary = readBase 2

readDecimal :: (Integral a) => String -> a
readDecimal = readBase 10

numDigits :: (Integral a, Show a, Integral b) => a -> b
numDigits = fromIntegral . length . decimal

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

primes :: [Integer]
primes = sieve [2..]
  where sieve [] = []
        sieve (x:xs) = x : sieve [n | n <- xs, n `mod` x > 0]

isPrime :: Integral a => a -> Bool
isPrime n = all (millerRabinPrimality (toInteger n)) $ take 10 primes

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll [] _ = True
containsAll (x:xs) l = x `elem` l && containsAll xs (delete x l)

is19Pandigital :: (Integral a, Show a) => a -> Bool
is19Pandigital = (>= 100000000) <&&> (< 1000000000) <&&> isPandigital

is09Pandigital :: (Integral a, Show a) => a -> Bool
is09Pandigital = (>= 1000000000) <&&> isPandigital

isPandigital :: (Integral a, Show a) => a -> Bool
isPandigital n = d <= 10 && containsAll (take d "1234567890") (decimal n)
  where d = numDigits n

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

squares :: [Integer]
squares = map (\n -> n*n) [0..]

triangleNums :: [Integer]
triangleNums = map (\n -> n*(n+1) `div` 2) [0..]

isTriangleNum :: Integral a => a -> Bool
isTriangleNum n = triangleNums `has` toInteger n

alphabetPosition :: Integral a => Char -> a
alphabetPosition c = fromIntegral $ (ord . toLower $ c) - ord 'a' + 1

wordValue :: String -> Integer
wordValue = sum . map alphabetPosition

isPerfectSquare :: Integral a => a -> Bool
isPerfectSquare n = squares `has` toInteger n

intSqrt :: Integral a => a -> Maybe a
intSqrt n = if isPerfectSquare n then
  fmap fromIntegral . elemIndex (toInteger n) $ squares else Nothing

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (comparing length) . group . sort
