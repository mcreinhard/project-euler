-- Problem 45
import Euler
import Data.List.Ordered

answer = (triangleNums `isect` pentagonalNums `isect` hexagonalNums) !! 3

main = print answer
