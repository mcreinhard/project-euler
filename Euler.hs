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
  isPerfectSquare,
  intSqrt,
  mostCommon
) where

import MillerRabin
import Numeric
import Data.Char
import Data.List
import Data.Maybe
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
primes = sieve [2..] where sieve [] = []
                           sieve (x:xs) = x : sieve [n | n <- xs, n `mod` x > 0]

isPrime :: Integral a => a -> Bool
isPrime n = all (millerRabinPrimality (toInteger n)) $ take 10 primes

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll [] _ = True
containsAll (x:xs) l = x `elem` l && containsAll xs (delete x l)

is19Pandigital :: (Integral a, Show a) => a -> Bool
is19Pandigital = (>= 100000000) <&&> isPandigital

isPandigital :: (Integral a, Show a) => a -> Bool
isPandigital n = d < 10 && containsAll (take d "123456789") (decimal n)
  where d = numDigits n

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

squares :: [Integer]
squares = map (\n -> n*n) [0..]

isPerfectSquare :: Integral a => a -> Bool
isPerfectSquare n = (== m) . fromJust . find (>= m) $ squares
  where m = toInteger n

intSqrt :: Integral a => a -> Maybe a
intSqrt n = if isPerfectSquare n then
  fmap fromIntegral . elemIndex (toInteger n) $ squares else Nothing

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (comparing length) . group . sort
