module Problem37 (answer) where
import Euler
import Control.Applicative

truncateLeft :: (Integral a, Show a) => a -> Maybe a
truncateLeft n
  | remainingDigits == "" = Nothing
  | otherwise             = Just (readDecimal remainingDigits)
  where remainingDigits = tail . decimal $ n

truncateRight :: (Integral a, Show a) => a -> Maybe a
truncateRight n
  | remainingDigits == "" = Nothing
  | otherwise             = Just (readDecimal remainingDigits)
  where remainingDigits = init . decimal $ n

isLRTruncatable :: Integer -> Bool
isLRTruncatable n = case truncateLeft n of
  Nothing -> isPrime n
  Just m -> isPrime n && isLRTruncatable m

isRLTruncatable :: Integer -> Bool
isRLTruncatable n = case truncateRight n of
  Nothing -> isPrime n
  Just m -> isPrime n && isRLTruncatable m

isTruncatable :: Integer -> Bool
isTruncatable = isLRTruncatable <&&> isRLTruncatable
  where (<&&>) = liftA2 (&&)

answer :: Integer
answer = sum . take 11 . filter isTruncatable . drop 4 $ primes
