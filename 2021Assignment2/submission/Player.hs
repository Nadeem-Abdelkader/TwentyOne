-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game

import Debug.Trace

-- You can add more imports if you need them

getMayBeValue :: Maybe Card  -> Card 
getMayBeValue x =
    case x of
          Nothing -> Card Heart Five 
          Just val -> val

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard Nothing _ _ _ _ _ = (Bid 100, "")
playCard dealerUpCard playersPoints playersHand myId myMemory myHand
    
    -- | trace ("bid: pid=" ++ show myId ++ " pinfo: " ++ show playersHand ++ " hand: " ++ show myHand) False = undefined 
    -- | trace ("Hand Value: " ++ show (handCalc myHand)) False = undefined 
    -- | trace ("Dealer: " ++ show (getRank (getMayBeValue dealerUpCard))) False = undefined 
    -- | trace ("My Id: " ++ show myId) False = undefined 
    -- | trace ("My Hand: " ++ show myHand) False = undefined 
    -- | trace ("My MY MY: " ++ show (Card Club Ace)) False = undefined 
    
    -- DEALER HAS AN ACE
    | getRank (getMayBeValue dealerUpCard) == Ace = (Hit, "")
    
    -- DEALER HAS A 10-CARD
    | (toPoints (getMayBeValue dealerUpCard) == 10) && (handCalc myHand == 10) = (Hit, "")
    | (toPoints (getMayBeValue dealerUpCard) == 10) && ((handCalc myHand >= 12) && (handCalc myHand <= 16)) = (Hit, "")
    | (toPoints (getMayBeValue dealerUpCard) == 10) && (handCalc myHand >= 17) = (Stand, "")
    
    -- DEALER HAS A 7,8, OR A 9
    | ((toPoints (getMayBeValue dealerUpCard) == 7) || (toPoints (getMayBeValue dealerUpCard) == 8) || (toPoints (getMayBeValue dealerUpCard) == 9)) && (handCalc myHand <= 9) = (Hit, "")
    | ((toPoints (getMayBeValue dealerUpCard) == 7) || (toPoints (getMayBeValue dealerUpCard) == 8) || (toPoints (getMayBeValue dealerUpCard) == 9)) && ((handCalc myHand >= 12) && (handCalc myHand <= 16)) = (Hit, "")
    | ((toPoints (getMayBeValue dealerUpCard) == 7) || (toPoints (getMayBeValue dealerUpCard) == 8) || (toPoints (getMayBeValue dealerUpCard) == 9)) && (handCalc myHand >= 17) = (Stand , "")
    
    -- DEALER HAS 4,5,6
    | ((toPoints (getMayBeValue dealerUpCard) == 4) || (toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand <= 8) = (Hit, "")
    | ((toPoints (getMayBeValue dealerUpCard) == 4) || (toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand >= 12) = (Stand , "")
    
    -- DEALER HAS A 3
    | (toPoints (getMayBeValue dealerUpCard) == 3) && (handCalc myHand <= 8) = (Hit, "")
    | (toPoints (getMayBeValue dealerUpCard) == 3) && (handCalc myHand == 12) = (Hit, "")
    | (toPoints (getMayBeValue dealerUpCard) == 3) && (handCalc myHand >= 13) = (Stand, "")
     
    -- DEALER HAS A 2
    | (toPoints (getMayBeValue dealerUpCard) == 2) && (handCalc myHand <= 9) = (Hit, "")
    | (toPoints (getMayBeValue dealerUpCard) == 2) && (handCalc myHand >= 13) = (Stand, "")
    
    -- DOUBLE DOWN
    -- | handCalc myHand == 10 = (DoubleDown 100, "")   
    -- | handCalc myHand == 11 = (DoubleDown 100, "")
    
    -- SPLIT

    -- SPLIT ON ACE'S
    | myHand == [Card Heart Ace, Card Diamond Ace] = (Split 100, "")
    | myHand == [Card Heart Ace, Card Spade Ace] = (Split 100, "")
    | myHand == [Card Heart Ace, Card Club Ace] = (Split 100, "")

    | myHand == [Card Diamond Ace, Card Heart Ace] = (Split 100, "")
    | myHand == [Card Diamond Ace, Card Spade Ace] = (Split 100, "")
    | myHand == [Card Diamond Ace, Card Club Ace] = (Split 100, "")

    | myHand == [Card Spade Ace, Card Heart Ace] = (Split 100, "")
    | myHand == [Card Spade Ace, Card Diamond Ace] = (Split 100, "")
    | myHand == [Card Spade Ace, Card Club Ace] = (Split 100, "")

    | myHand == [Card Club Ace, Card Heart Ace] = (Split 100, "")
    | myHand == [Card Club Ace, Card Diamond Ace] = (Split 100, "")
    | myHand == [Card Club Ace, Card Spade Ace] = (Split 100, "")

    -- SPLIT ON 8'S
    | myHand == [Card Heart Eight, Card Diamond Eight] = (Split 100, "")
    | myHand == [Card Heart Eight, Card Spade Eight] = (Split 100, "")
    | myHand == [Card Heart Eight, Card Club Eight] = (Split 100, "")

    | myHand == [Card Diamond Eight, Card Heart Eight] = (Split 100, "")
    | myHand == [Card Diamond Eight, Card Spade Eight] = (Split 100, "")
    | myHand == [Card Diamond Eight, Card Club Eight] = (Split 100, "")

    | myHand == [Card Spade Eight, Card Heart Eight] = (Split 100, "")
    | myHand == [Card Spade Eight, Card Diamond Eight] = (Split 100, "")
    | myHand == [Card Spade Eight, Card Club Eight] = (Split 100, "")

    | myHand == [Card Club Eight, Card Heart Eight] = (Split 100, "")
    | myHand == [Card Club Eight, Card Diamond Eight] = (Split 100, "")
    | myHand == [Card Club Eight, Card Spade Eight] = (Split 100, "")
    
    | otherwise = (Stand, "")