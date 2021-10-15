-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game
import Data.Maybe (isNothing)
import qualified Player

-- You can add more imports if you need them
notHeartCards :: [Card] -> [Card]
notHeartCards [] = []
notHeartCards cards = filter (\x -> getSuit(x) /= Heart) cards

getSuit :: Card -> Suit
getSuit (Card suit _) = suit

cardRankSort :: [Card] -> [Card]
cardRankSort [] = []
cardRankSort (pivot:rest) = (cardRankSort lesser) ++ [pivot] ++ (cardRankSort greater)
    where
        lesser = filter (\x -> getRank x < getRank pivot) rest
        greater = filter (\x -> getRank x >= getRank pivot) rest

retCardNoLead :: [Card] -> Card
retCardNoLead cinhand 
    | length notHearts > 0 = head $ cardRankSort $ notHearts
    | otherwise = head $ cinhand
    where
        notHearts = notHeartCards cinhand

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard card pts info id memo hand
    | isNothing card = (Bid 10, "")
    | Player.playCard == 2 = (DoubleDown 20,"")
    | otherwise  = (Stand, "")