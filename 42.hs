-- Problem 42
import Euler
import Data.List.Split

numTriangleWords :: [String] -> Integer
numTriangleWords = toInteger . length . filter (isTriangleNum . wordValue)

answer :: String -> Integer
answer = numTriangleWords . wordsBy (`elem` "\",")

main = readFile "resources/p042_words.txt" >>= print . answer
