-- Problem 69
import Euler

totientQuotient :: Integer -> Double
totientQuotient n = fromIntegral n / fromIntegral (totient n)

answer :: Integer
answer = argmax totientQuotient [1..1000000]

main :: IO ()
main = print answer
