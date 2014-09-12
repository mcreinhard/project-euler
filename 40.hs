-- Problem 40
import Euler
import Data.Char

answer = product . map (\n -> digitToInt $ digits !! (10^n)) $ [0..6]
  where digits = concatMap decimal [0..]

main = print answer
