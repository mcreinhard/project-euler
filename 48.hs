-- Problem 48
import Euler

answer = sum [n^n | n <- [1..1000]] `mod` 10^10

main = print answer
