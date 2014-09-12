-- Problem 43
import Euler
import Data.List
import Data.Maybe

digits = "0123456789"

-- Stage n gives all (n+3)-digit strings satisfying the first (n+1)
-- divisibility conditions.
stage :: Int -> [String]
stage 0 = [[a,b,c] | a <- digits, b <- digits, c <- digits,
           2 `divides` readDecimal [a,b,c], not . hasDuplicates $ [a,b,c]]
stage n = [ds ++ [d] | ds <- stage (n-1), d <- digits,
           (primes !! n) `divides` readDecimal (drop n $ ds ++ [d]),
           not . hasDuplicates $ ds ++ [d]]

addFirstDigit :: String -> Integer
addFirstDigit ds = readDecimal $ firstDigit:ds
  where firstDigit = fromJust $ find (`notElem` ds) digits

answer = sum . map addFirstDigit $ stage 6

main = print answer
