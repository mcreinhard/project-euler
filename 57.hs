-- Problem 57
import Euler
import Data.Ratio
import Data.Function
import Control.Applicative

f :: Rational -> Rational
f x = 1+1/(1+x)

answer = length . filter (moreDigits <$> numerator <*> denominator)
  $ take 1000 iterations
    where moreDigits = (>) `on` numDigits
          iterations = tail $ iterate f 1

main = print answer

