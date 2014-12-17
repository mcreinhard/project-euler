module Euler (
  base, binary, decimal, readBase, readBinary, readDecimal, numDigits, digits,
  primes, isPrime, primeFactors, numUniquePrimeFactors,
  intSqrt, isPerfectSquare, triangleNums, isTriangleNum,
  pentagonalNums, isPentagonal, hexagonalNums,
  isPalindrome, isPermutation,
  isPandigital, is19Pandigital, is09Pandigital,
  mostCommon, hasDuplicates,
  alphabetPosition, wordValue,
  divides, totient,
  (<==>), (</=>), (<&&>), (<||>), (<+>),
  argmax, argmin
) where

import MillerRabin
import Numeric
import Data.Char
import Data.List
import Data.List.Ordered
import Data.Ord
import Data.MemoTrie
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

digits :: (Integral a, Show a) => a -> [a]
digits = map (fromIntegral . digitToInt) . decimal

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

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation s t = case s of
  [] ->  null t
  x:xs -> (x `elem` t) && isPermutation xs (delete x t)

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

totient :: (Integral a, HasTrie a) => a -> a
totient = memo phi where
  phi k
    | k == 1    = 1
    | isPrime k = k-1
    | otherwise = totient m * totient n * d `div` totient d
    where m = head . filter (`divides` k) $ primes
          n = k `div` m
          d = gcd m n

infix 4 <==>
(<==>) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(<==>) = liftA2 (==)

infix 4 </=>
(</=>) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(</=>) = liftA2 (/=)

infixr 3 <&&>
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

infixr 2 <||>
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

infixl 6 <+>
(<+>) :: (Applicative f, Num a) => f a -> f a -> f a
(<+>) = liftA2 (+)

argmax :: Ord b => (a -> b) -> [a] -> a
argmax f = foldl1 maxBy
  where maxBy x y = if f x >= f y then x else y

argmin :: Ord b => (a -> b) -> [a] -> a
argmin f = foldl1 minBy
  where minBy x y = if f x <= f y then x else y
