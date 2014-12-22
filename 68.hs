-- Problem 68
import Euler
import Data.List
import Data.Maybe
import Control.Monad

strings :: [String]
strings = do 
    perm <- permutations [1..10]
    let (insides, outsides) = splitAt 5 perm
        sumLines = [[outsides!!n, insides!!n, insides!!((n+1) `mod` 5)] 
                    | n<-[0..4]]
        total    = sum $ head sumLines
    guard $ all (== total) (map sum sumLines)
    let minOutside = minimum $ map head sumLines
        offset     = fromJust $ findIndex (head <==> const minOutside) sumLines
        rotated    = rotate offset sumLines
    return $ concatMap show (concat rotated)

answer :: String
answer = maximum strings

main = print answer

