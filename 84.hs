-- Problem 84
import Euler
import Data.List
import Data.Maybe
import Control.Monad
import Numeric.Probability.Distribution

data Square = GO|A1|CC1|A2|T1|R1|B1|CH1|B2|B3   -- Row 1
              |JAIL|C1|U1|C2|C3|R2|D1|CC2|D2|D3 -- Row 2
              |FP|E1|CH2|E2|E3|R3|F1|F2|U2|F3   -- Row 3
              |G2J|G1|G2|CC3|G3|R4|CH3|H1|T2|H2 -- Row 4
    deriving (Read, Show, Enum, Eq, Ord)

isCC :: Square -> Bool
isCC = (`elem` [CC1,CC2,CC3])

isCH :: Square -> Bool
isCH = (`elem` [CH1,CH2,CH3])

isR :: Square -> Bool
isR = (`elem` [R1,R2,R3,R4])

isU :: Square -> Bool
isU = (`elem` [U1,U2])

offsetBy :: Int -> Square -> Square
offsetBy n = toEnum . (`mod` boardSize) . (+ n) . fromEnum

nextR :: Square -> Square
nextR = fromJust . find isR . iterate (offsetBy 1)

nextU :: Square -> Square
nextU = fromJust . find isU . iterate (offsetBy 1)

boardSize :: Int
boardSize = 40

dieSize :: Int
dieSize = 6

dieDist :: T Double Int
dieDist = uniform [1..dieSize]

twoDiceDist :: T Double Int
twoDiceDist = dieDist <+> dieDist

movementDist :: Square -> T Double Square
movementDist sq = let threeDoubles = 1 / fromIntegral (dieSize^3) in
    do roll <- twoDiceDist
       fromFreqs [(JAIL,threeDoubles),
                  (offsetBy roll sq,1-threeDoubles)]

redirectDist :: Square -> T Double Square
redirectDist sq
    | isCC sq   = ccDist
    | isCH sq   = chDist
    | sq == G2J = certainly JAIL
    | otherwise = certainly sq
    where ccDist = uniform $ [GO,JAIL] ++ replicate 14 sq
          chDist = uniform $ [GO,JAIL,C1,E3,H2,R1,nextR sq,nextR sq,
                              nextU sq,offsetBy (-3) sq] ++ replicate 6 sq

nextSquareDist :: Square -> T Double Square
nextSquareDist = movementDist >=> redirectDist

main :: IO ()
main = let iterations = iterate (norm . (>>= nextSquareDist)) (uniform [GO .. H2]) in
       print . drop 36 . sortP . decons $ (iterations !! 200)
