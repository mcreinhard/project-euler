-- Problem 65
import Euler
import Data.List
import Data.Ratio

data ContinuedFrac = CF Integer [Integer]

instance Show ContinuedFrac where
  show (CF n ns) = "[" ++ show n ++ ";"
    ++ intercalate "," (map show $ take 10 ns) ++ "...]"

evaluate :: ContinuedFrac -> Rational
evaluate (CF m ms) = m%1 + case ms of
  [] -> 0
  n:ns -> 1 / evaluate (CF n ns)

convergent :: Integer -> ContinuedFrac -> Rational
convergent n (CF m ms) = evaluate $ CF m (take (fromIntegral n-1) ms)

e :: ContinuedFrac
e = CF 2 $ concat [[1,2*k,1] | k<-[1..]]

answer :: Integer
answer = sum . digits . numerator $ convergent 100 e

main :: IO ()
main = print answer
