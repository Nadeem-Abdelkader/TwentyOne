-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game

import Debug.Trace
import Data.Vector.Storable (create)

-- You can add more imports if you need them

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard Nothing _ _ _ _ myHand = createSuitableBid myHand
playCard dealerUpCard playersPoints playersHand myId myMemory myHand
    -- TRACE
    -- | trace ("bid: pid=" ++ show myId ++ " pinfo: " ++ show playersHand ++ " hand: " ++ show myHand) False = undefined 
    -- | trace ("Hand Value: " ++ show (handCalc myHand)) False = undefined 
    -- | trace ("Dealer: " ++ show (getRank (getMayBeValue dealerUpCard))) False = undefined
    
    -- INSURANCE
    | insuranceHand dealerUpCard myMemory = (Insurance 50, "")

    -- SPLIT
    | splitHand myHand = (Split 100, "")

    -- DOUBLE DOWN
    -- | doubleDownHand dealerUpCard myHand = (DoubleDown 100, "")

    -- HIT
    | hitHand dealerUpCard myHand = (Hit, "")

    -- STAND
    | standHand dealerUpCard myHand = (Stand, "")
    
    | otherwise = (Stand, "")

getMayBeValue :: Maybe Card  -> Card
getMayBeValue x =
    case x of
          Nothing -> Card Heart Five
          Just val -> val

createSuitableBid :: Hand -> (Action , String)
createSuitableBid currentHand = (Bid 100, "FIRST TURN AFTER BIDDING")

splitHand :: Hand -> Bool
splitHand currentHand
    | length currentHand /= 2 = False

    -- SPLIT ON ACE'S
    | currentHand == [Card Heart Ace, Card Diamond Ace] = True
    | currentHand == [Card Heart Ace, Card Spade Ace] = True
    | currentHand == [Card Heart Ace, Card Club Ace] = True

    | currentHand == [Card Diamond Ace, Card Heart Ace] = True
    | currentHand == [Card Diamond Ace, Card Spade Ace] = True
    | currentHand == [Card Diamond Ace, Card Club Ace] = True

    | currentHand == [Card Spade Ace, Card Heart Ace] = True
    | currentHand == [Card Spade Ace, Card Diamond Ace] = True
    | currentHand == [Card Spade Ace, Card Club Ace] = True

    | currentHand == [Card Club Ace, Card Heart Ace] = True
    | currentHand == [Card Club Ace, Card Diamond Ace] = True
    | currentHand == [Card Club Ace, Card Spade Ace] = True

    -- SPLIT ON 8'S
    | currentHand == [Card Heart Eight, Card Diamond Eight ] = True
    | currentHand == [Card Heart Eight, Card Spade Eight] = True
    | currentHand == [Card Heart Eight, Card Club Eight] = True

    | currentHand == [Card Diamond Eight, Card Heart Eight] = True
    | currentHand == [Card Diamond Eight, Card Spade Eight] = True
    | currentHand == [Card Diamond Eight, Card Club Eight] = True

    | currentHand == [Card Spade Eight, Card Heart Eight] = True
    | currentHand == [Card Spade Eight, Card Diamond Eight] = True
    | currentHand == [Card Spade Eight, Card Club Eight] = True

    | currentHand == [Card Club Eight, Card Heart Eight] = True
    | currentHand == [Card Club Eight, Card Diamond Eight] = True
    | currentHand == [Card Club Eight, Card Spade Eight] = True

    | otherwise = False

hitHand :: Maybe Card -> Hand -> Bool
hitHand dealerUpCard myHand
    | getRank (getMayBeValue dealerUpCard) == Ace  && (handCalc myHand <= 17) = True
    | (toPoints (getMayBeValue dealerUpCard) == 10) && (handCalc myHand == 10) = True
    | (toPoints (getMayBeValue dealerUpCard) == 10) && ((handCalc myHand >= 12) && (handCalc myHand <= 16)) = True
    | ((toPoints (getMayBeValue dealerUpCard) == 7) || (toPoints (getMayBeValue dealerUpCard) == 8) || (toPoints (getMayBeValue dealerUpCard) == 9)) && (handCalc myHand <= 9) = True
    | ((toPoints (getMayBeValue dealerUpCard) == 7) || (toPoints (getMayBeValue dealerUpCard) == 8) || (toPoints (getMayBeValue dealerUpCard) == 9)) && ((handCalc myHand >= 12) && (handCalc myHand <= 16)) = True
    | ((toPoints (getMayBeValue dealerUpCard) == 4) || (toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand <= 8) = True
    | (toPoints (getMayBeValue dealerUpCard) == 3) && (handCalc myHand <= 8) = True
    | (toPoints (getMayBeValue dealerUpCard) == 3) && (handCalc myHand == 12) = True
    | (toPoints (getMayBeValue dealerUpCard) == 2) && (handCalc myHand <= 9) = True
    | otherwise = False

standHand :: Maybe Card -> Hand -> Bool
standHand dealerUpCard myHand
    | (toPoints (getMayBeValue dealerUpCard) == 10) && (handCalc myHand >= 17) = True
    | ((toPoints (getMayBeValue dealerUpCard) == 7) || (toPoints (getMayBeValue dealerUpCard) == 8) || (toPoints (getMayBeValue dealerUpCard) == 9)) && (handCalc myHand >= 17) = True
    | ((toPoints (getMayBeValue dealerUpCard) == 4) || (toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand >= 12) = True
    | (toPoints (getMayBeValue dealerUpCard) == 3) && (handCalc myHand >= 13) = True 
    | (toPoints (getMayBeValue dealerUpCard) == 2) && (handCalc myHand >= 13) = True
    | otherwise = False

doubleDownHand :: Maybe Card -> Hand -> Bool
doubleDownHand dealerUpCard myHand
    | length myHand /= 2 = False
    | handCalc myHand == 10 = True  
    | handCalc myHand == 11 = True
    | ((toPoints (getMayBeValue dealerUpCard) == 4) || (toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand == 10) = True   
    | ((toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand == 9) = True
    | otherwise = False 

insuranceHand :: Maybe Card -> Maybe String -> Bool
insuranceHand dealerUpCard myMemory
    | getRank (getMayBeValue dealerUpCard) == Ace  && myMemory == Just "FIRST TURN AFTER BIDDING" = True
    | otherwise = False