-- Problem 42
import Euler
import Data.List.Split

numTriangleWords :: [String] -> Integer
numTriangleWords = toInteger . length . filter (isTriangleNum . wordValue)

main = readFile "resources/p042_words.txt"
  >>= print . numTriangleWords . wordsBy (`elem` "\",")
