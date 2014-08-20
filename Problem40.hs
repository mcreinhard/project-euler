module Problem40 (answer) where
import Euler
import Data.Char

answer :: Integer
answer = toInteger . product . map (\n -> digitToInt $ digits !! (10^n)) $ [0..6]
  where digits = concatMap decimal [0..]
