module Euler (
  base,
  binary,
  decimal,
  readBase,
  readBinary,
  readDecimal,
  isPalindrome,
  primes,
  isPrime,
  isPandigital
) where

import Numeric
import Data.Char
import Data.List
import Data.Maybe
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

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

primes :: [Integer]
primes = sieve [2..] where sieve [] = []
                           sieve (x:xs) = x : sieve [n | n <- xs, n `mod` x > 0]

isPrime :: Integral a => a -> Bool
isPrime = (isPrimeList !!) . fromIntegral

isPrimeList :: [Bool]
isPrimeList = map inPrimes [0..]
  where inPrimes n = (== n) . fromJust . find (>= n) $ primes

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll [] _ = True
containsAll (x:xs) l = x `elem` l && containsAll xs (delete x l)

isPandigital :: (Integral a, Show a) => a -> Bool
isPandigital = (containsAll "123456789" . decimal) <&&> (< 1000000000)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
