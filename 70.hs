-- Problem 70
import Euler
import Data.Function
import Control.Applicative

totientQuotient :: Integer -> Double
totientQuotient n = fromIntegral n / fromIntegral (totient n)

searchSpace :: [Integer]
searchSpace = filter conditions [2..9999999] where
  conditions x = all not (divides <$> [2,3,5,7,11,13] <*> pure x) 
                   && not (isPrime x)

permutables :: [Integer]
permutables = 
  filter ((isPermutation `on` decimal) <$> totient <*> id) searchSpace

answer :: Integer
answer = argmin totientQuotient permutables

main :: IO ()
main = print answer
