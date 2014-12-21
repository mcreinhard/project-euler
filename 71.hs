-- Problem 71
import Data.Ratio

closestOnLeft :: Rational -> Integer -> Rational
closestOnLeft q newDenom = 
    let newNumer = (newDenom * numerator q) `div` denominator q
    in  if newNumer % newDenom == q then (newNumer-1) % newDenom
                                    else newNumer     % newDenom

answer :: Integer
answer = numerator . maximum . map (closestOnLeft (3%7)) $ [1..1000000]

main = print answer
