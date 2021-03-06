-- Problem 39
import Euler

rightTriangles :: [[Integer]]
rightTriangles = [[a,b,c] | c <- [1..1000], b <- [1..c],
                   a <- [1..b], a*a + b*b == c*c, a+b+c <= 1000]

answer = mostCommon . filter (<= 1000) . map sum $ rightTriangles

main = print answer
