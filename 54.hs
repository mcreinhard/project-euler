-- Problem 54
import Euler
import Control.Applicative
import Control.Monad
import Data.List
import Data.Function
import Data.Maybe

data CardSuit = Hearts | Diamonds | Clubs | Spades
  deriving (Read, Show, Eq)

data CardValue = Two | Three | Four | Five
  | Six | Seven | Eight | Nine | Ten
  | Jack | Queen | King | Ace
  deriving (Read, Show, Enum, Eq, Ord, Bounded)

data Card = Card {value :: CardValue, suit :: CardSuit}
  deriving (Read, Show, Eq)

instance Ord Card where
  compare = compare `on` value

data PokerRank = HighCard | OnePair | TwoPairs | ThreeOfAKind | Straight
  | Flush | FullHouse | FourOfAKind | StraightFlush
  deriving (Read, Show, Enum, Eq, Ord, Bounded)

newtype Hand = Hand {cards :: [Card]}
  deriving (Read, Show, Eq)

instance Ord Hand where
  compare = compare `on` rankWithCards

ranks :: [PokerRank]
ranks = reverse [HighCard .. StraightFlush]

charToCardSuit :: Char -> Maybe CardSuit
charToCardSuit  = (`lookup` stringValues) where
  stringValues = [('H',Hearts),('D',Diamonds),('C',Clubs),('S',Spades)]

charToCardValue :: Char -> Maybe CardValue
charToCardValue = (`lookup` stringValues) where
  stringValues = [('2',Two),('3',Three),('4',Four),('5',Five),('6',Six),
                  ('7',Seven),('8',Eight),('9',Nine),('T',Ten),('J',Jack),
                  ('Q',Queen), ('K',King),('A',Ace)]

stringToCard :: String -> Card
stringToCard s = fromMaybe (error "Invalid card input") $ do
  guard $ length s == 2
  charValue <- charToCardValue $ head s
  charSuit  <- charToCardSuit  $ last s
  return $ Card charValue charSuit

handByValue :: Hand -> [[Card]]
handByValue = groupBy ((==) `on` value) . sort . cards

isStraight :: Hand -> Bool
isStraight = isConsecutive . sort . map value . cards
  where isConsecutive xs = xs == take 5 [(head xs)..]

isFlush :: Hand -> Bool
isFlush = (== 1) . length . handBySuit
  where handBySuit = groupBy ((==) `on` suit) . cards

highCardsInRank :: Hand -> PokerRank -> Maybe [Card]
highCardsInRank hand rank = case rank of
  OnePair  -> highCardsInOfKind 1 2
  TwoPairs -> highCardsInOfKind 2 2
  ThreeOfAKind -> highCardsInOfKind 1 3
  FourOfAKind -> highCardsInOfKind 1 4
  FullHouse -> do
    let groupedHand = handByValue hand
    guard $ sort (map length groupedHand) == [2,3]
    return . reverse $ map head groupedHand
  _ -> guard (test hand) >> return []
  where highCardsInOfKind numTuples size = do
          let tuples = size `ofKind` hand
          guard (length tuples >= numTuples)
          return . sort . map head $ tuples
            where ofKind n = filter ((>= n) . length) . handByValue
        test = case rank of
          HighCard -> const True
          Straight -> isStraight
          Flush -> isFlush
          StraightFlush -> isStraight <&&> isFlush
          _ -> const False

rankWithCards :: Hand -> ((PokerRank, [Card]), [Card])
rankWithCards hand = (rankAndHighCards, allCards)
  where rankAndHighCards = head . catMaybes
          $ [(,) rank <$> highCardsInRank hand rank | rank <- ranks]
        allCards = sortBy (flip compare) $ cards hand

playerOneWins :: String -> Bool
playerOneWins = uncurry ((>) `on` handFromStrings) . splitAt 5 . words
  where handFromStrings = Hand . map stringToCard

answer :: String -> Integer
answer s = toInteger . length . filter playerOneWins $ lines s

main :: IO ()
main = do
  input <- readFile "resources/p054_poker.txt"
  print $ answer input
