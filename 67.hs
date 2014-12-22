-- Problem 67
import Euler
import Data.MemoTrie

maxPathSum :: [[Int]] -> Int -> Int -> Int
maxPathSum = memo3 maxPathSum' where
maxPathSum' tri i j
    | i >= length tri = 0
    | otherwise       = (tri!!i)!!j
          + max (maxPathSum tri(i+1) j) (maxPathSum tri(i+1) (j+1))

answer :: String -> Int
answer = (\tri -> maxPathSum tri 0 0) . map (map readDecimal . words) . lines

main = readFile "resources/p067_triangle.txt" >>= print . answer
