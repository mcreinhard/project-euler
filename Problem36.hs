module Problem36 (answer) where
import Euler

isDoubleBasePalindrome :: Integer -> Bool
isDoubleBasePalindrome n =
  (isPalindrome . binary $ n) && (isPalindrome . decimal $ n)

answer :: Integer
answer = sum . filter isDoubleBasePalindrome $ [1..999999]
