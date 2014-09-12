-- Problem 36
import Euler

isDoubleBasePalindrome :: Integer -> Bool
isDoubleBasePalindrome n =
  (isPalindrome . binary $ n) && (isPalindrome . decimal $ n)

answer = sum . filter isDoubleBasePalindrome $ [1..999999]

main = print answer
