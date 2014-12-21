-- Problem 72
import Euler

answer :: Int
answer = sum . map totient $ [2..1000000]

main = print answer
