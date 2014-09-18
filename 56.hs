-- Problem 56
import Euler

answer :: Integer
answer = maximum [sum . digits $ a^b | a<-[1..99], b<-[1..99]]

main :: IO ()
main = print answer
