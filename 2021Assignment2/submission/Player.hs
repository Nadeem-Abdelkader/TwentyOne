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
    | insuranceHand dealerUpCard myMemory = (Insurance (maxInsure maxBid ), "")

    -- SPLIT
    | splitHand dealerUpCard myHand = (Split maxBid, "")

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
createSuitableBid currentHand = (Bid maxBid, "FIRST TURN AFTER BIDDING")

splitHand :: Maybe Card -> Hand -> Bool
splitHand dealerUpCard currentHand
    | length currentHand /= 2 = False

    -- SPLIT ON ACE'S
    -- | trace ("Hand : " ++ show (currentHand)) False = undefined 
    -- | trace ("1st : " ++ show (getRank (head currentHand) == Ace)) False = undefined 
    -- | trace ("2nd : " ++ show (getRank (head (tail currentHand)) == Ace)) False = undefined 
    -- | trace ("Both : " ++ show (getRank (head currentHand) == Ace && getRank (head (tail currentHand)) == Ace)) False = undefined 
    | getRank (head currentHand) == Ace && getRank (head (tail currentHand)) == Ace = True
    -- | currentHand == [Card Heart Ace, Card Diamond Ace] = True
    -- | currentHand == [Card Heart Ace, Card Spade Ace] = True
    -- | currentHand == [Card Heart Ace, Card Club Ace] = True

    -- | currentHand == [Card Diamond Ace, Card Heart Ace] = True
    -- | currentHand == [Card Diamond Ace, Card Spade Ace] = True
    -- | currentHand == [Card Diamond Ace, Card Club Ace] = True

    -- | currentHand == [Card Spade Ace, Card Heart Ace] = True
    -- | currentHand == [Card Spade Ace, Card Diamond Ace] = True
    -- | currentHand == [Card Spade Ace, Card Club Ace] = True

    -- | currentHand == [Card Club Ace, Card Heart Ace] = True
    -- | currentHand == [Card Club Ace, Card Diamond Ace] = True
    -- | currentHand == [Card Club Ace, Card Spade Ace] = True

    -- SPLIT ON 8'S
    | getRank (head currentHand) == Eight && getRank (head (tail currentHand)) == Eight = True
    -- | currentHand == [Card Heart Eight, Card Diamond Eight ] = True
    -- | currentHand == [Card Heart Eight, Card Spade Eight] = True
    -- | currentHand == [Card Heart Eight, Card Club Eight] = True

    -- | currentHand == [Card Diamond Eight, Card Heart Eight] = True
    -- | currentHand == [Card Diamond Eight, Card Spade Eight] = True
    -- | currentHand == [Card Diamond Eight, Card Club Eight] = True

    -- | currentHand == [Card Spade Eight, Card Heart Eight] = True
    -- | currentHand == [Card Spade Eight, Card Diamond Eight] = True
    -- | currentHand == [Card Spade Eight, Card Club Eight] = True

    -- | currentHand == [Card Club Eight, Card Heart Eight] = True
    -- | currentHand == [Card Club Eight, Card Diamond Eight] = True
    -- | currentHand == [Card Club Eight, Card Spade Eight] = True

    -- SPLIT ON 7'S FOR DEALER 2 - 7
    | getRank (head currentHand) == Seven && getRank (head (tail currentHand)) == Seven  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7)
    | getRank (head currentHand) == Two && getRank (head (tail currentHand)) == Two  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7)
    | getRank (head currentHand) == Three && getRank (head (tail currentHand)) == Three  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7)

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Heart Seven , Card Diamond Seven ] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Heart Seven, Card Spade Seven] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Heart Seven, Card Club Seven] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Diamond Seven, Card Heart Seven] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Diamond Seven, Card Spade Seven] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Diamond Seven, Card Club Seven] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Spade Seven, Card Heart Seven] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Spade Seven, Card Diamond Seven] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Spade Seven, Card Club Seven] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Club Seven, Card Heart Seven] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Club Seven, Card Diamond Seven] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Club Seven, Card Spade Seven] = True

-- SPLIT ON 2'S FOR DEALER 2 - 7
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Heart Two , Card Diamond Two ] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Heart Two, Card Spade Two] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Heart Two, Card Club Two] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Diamond Two, Card Heart Two] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Diamond Two, Card Spade Two] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Diamond Two, Card Club Two] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Spade Two, Card Heart Two] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Spade Two, Card Diamond Two] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Spade Two, Card Club Two] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Club Two, Card Heart Two] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Club Two, Card Diamond Two] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Club Two, Card Spade Two] = True

-- SPLIT ON 3'S FOR DEALER 2 - 7
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Heart Three  , Card Diamond Three ] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Heart Three, Card Spade Three] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Heart Three, Card Club Three] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Diamond Three, Card Heart Three] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Diamond Three, Card Spade Three] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Diamond Three, Card Club Three] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Spade Three, Card Heart Three] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Spade Three, Card Diamond Three] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Spade Three, Card Club Three] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Club Three, Card Heart Three] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Club Three, Card Diamond Three] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) && currentHand == [Card Club Three, Card Spade Three] = True

-- SPLIT ON 6'S FOR DEALER 2 - 6
    | getRank (head currentHand) == Six && getRank (head (tail currentHand)) == Six  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6)
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Heart Six, Card Diamond Six ] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Heart Six, Card Spade Six] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Heart Six, Card Club Six] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Diamond Six, Card Heart Six] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Diamond Six, Card Spade Six] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Diamond Six, Card Club Six] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Spade Six, Card Heart Six] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Spade Six, Card Diamond Six] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Spade Six, Card Club Six] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Club Six, Card Heart Six] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Club Six, Card Diamond Six] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Club Six, Card Spade Six] = True

-- SPLIT ON 5'S FOR DEALER 2 - 9
    | getRank (head currentHand) == Five && getRank (head (tail currentHand)) == Five  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9)

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && currentHand == [Card Heart Five , Card Diamond Five ] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && currentHand == [Card Heart Five, Card Spade Five] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && currentHand == [Card Heart Five, Card Club Five] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && currentHand == [Card Diamond Five, Card Heart Five] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && currentHand == [Card Diamond Five, Card Spade Five] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && currentHand == [Card Diamond Five, Card Club Five] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && currentHand == [Card Spade Five, Card Heart Five] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && currentHand == [Card Spade Five, Card Diamond Five] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && currentHand == [Card Spade Five, Card Club Five] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && currentHand == [Card Club Five, Card Heart Five] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && currentHand == [Card Club Five, Card Diamond Five] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && currentHand == [Card Club Five, Card Spade Five] = True

-- SPLIT ON 4'S FOR DEALER 5 - 6
    | getRank (head currentHand) == Four && getRank (head (tail currentHand)) == Four  = (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6)

    -- | (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Heart Four, Card Diamond Four ] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Heart Four, Card Spade Four] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Heart Four, Card Club Four] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Diamond Four, Card Heart Four] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Diamond Four, Card Spade Four] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Diamond Four, Card Club Four] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Spade Four, Card Heart Four] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Spade Four, Card Diamond Four] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Spade Four, Card Club Four] = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Club Four, Card Heart Four] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Club Four, Card Diamond Four] = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6) && currentHand == [Card Club Four, Card Spade Four] = True

    -- SPLIT ON 9'S for dealer 2 - 9 except 7
    | getRank (head currentHand) == Nine && getRank (head (tail currentHand)) == Nine  = not ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && (toPoints (getMayBeValue dealerUpCard) /= 7))

    -- | not ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && (toPoints (getMayBeValue dealerUpCard) /= 7)) = False
    -- | currentHand == [Card Heart Nine, Card Diamond Nine ] = True
    -- | currentHand == [Card Heart Nine, Card Spade Nine] = True
    -- | currentHand == [Card Heart Nine, Card Club Nine] = True

    -- | currentHand == [Card Diamond Nine, Card Heart Nine] = True
    -- | currentHand == [Card Diamond Nine, Card Spade Nine] = True
    -- | currentHand == [Card Diamond Nine, Card Club Nine] = True

    -- | currentHand == [Card Spade Nine, Card Heart Nine] = True
    -- | currentHand == [Card Spade Nine, Card Diamond Nine] = True
    -- | currentHand == [Card Spade Nine, Card Club Nine] = True

    -- | currentHand == [Card Club Nine, Card Heart Nine] = True
    -- | currentHand == [Card Club Nine, Card Diamond Nine] = True
    -- | currentHand == [Card Club Nine, Card Spade Nine] = True

    | otherwise = False

-- splitOnSevensTwosThrees :: Maybe Card -> Bool
-- splitOnSevensTwosThrees dealerUpCard
--     | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7) = True
--     | otherwise = False 

hitHand :: Maybe Card -> Hand -> Bool
hitHand dealerUpCard myHand
    | handCalc myHand == 8 = True
    
    -- SOFT 18 TO DEALER 9 - ACE
    | (handCalc myHand == 18) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && (toPoints (getMayBeValue dealerUpCard) >= 9) && (toPoints (getMayBeValue dealerUpCard) <= 11) = True

    -- SOFT 17 TO DEALER NOT 3 - 6
    | (handCalc myHand == 17) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && not ((toPoints (getMayBeValue dealerUpCard) >= 3) && (toPoints (getMayBeValue dealerUpCard) <= 6)) = True

    -- SOFT 16 TO DEALER NOT 4 - 6
    | (handCalc myHand == 16) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && not ((toPoints (getMayBeValue dealerUpCard) >= 4) && (toPoints (getMayBeValue dealerUpCard) <= 6)) = True

    -- SOFT 15 TO DEALER NOT 4 - 6
    | (handCalc myHand == 17) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && not ((toPoints (getMayBeValue dealerUpCard) >= 4) && (toPoints (getMayBeValue dealerUpCard) <= 6)) = True

    -- SOFT 14 TO DEALER NOT 5 - 6
    | (handCalc myHand == 15) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && not ((toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6)) = True

    -- SOFT 13 TO DEALER NOT 5 - 6
    | (handCalc myHand == 15) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && not ((toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6)) = True

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
    | handCalc myHand >= 17 = True
    -- SOFT 20
    | (handCalc myHand == 20) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) = True

    -- SOFT 19 TO DEALER NOT 6
    | (handCalc myHand == 19) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && (toPoints (getMayBeValue dealerUpCard) /= 6) = True

    -- SOFT 18 TO DEALER NOT 2-6 AND NOT 9 - ACE
    | (handCalc myHand == 18) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && not ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6)) && (toPoints (getMayBeValue dealerUpCard) >= 9) = True

    | (toPoints (getMayBeValue dealerUpCard) == 10) && (handCalc myHand >= 17) = True
    | ((toPoints (getMayBeValue dealerUpCard) == 7) || (toPoints (getMayBeValue dealerUpCard) == 8) || (toPoints (getMayBeValue dealerUpCard) == 9)) && (handCalc myHand >= 17) = True
    | ((toPoints (getMayBeValue dealerUpCard) == 4) || (toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand >= 12) = True
    | (toPoints (getMayBeValue dealerUpCard) == 3) && (handCalc myHand >= 13) = True 
    | (toPoints (getMayBeValue dealerUpCard) == 2) && (handCalc myHand >= 13) = True
    | ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6)) && (handCalc myHand == 16) = True
    | ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6)) && (handCalc myHand == 15) = True
    | ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6)) && (handCalc myHand == 14) = True
    | ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6)) && (handCalc myHand == 13) = True
    | ((toPoints (getMayBeValue dealerUpCard) >= 4) && (toPoints (getMayBeValue dealerUpCard) <= 6)) && (handCalc myHand == 12) = True

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && handCalc myHand == 16 = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && handCalc myHand == 15 = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && handCalc myHand == 14 = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6) && handCalc myHand == 13 = True
    -- | (toPoints (getMayBeValue dealerUpCard) >= 4) && (toPoints (getMayBeValue dealerUpCard) <= 6) && handCalc myHand == 12 = True
    | otherwise = False

doubleDownHand :: Maybe Card -> Hand -> Bool
doubleDownHand dealerUpCard myHand
    | length myHand /= 2 = False
    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && handCalc myHand == 10 = True  
    -- | (toPoints (getMayBeValue dealerUpCard) >= 3) && (toPoints (getMayBeValue dealerUpCard) <= 6) && handCalc myHand == 9 = True  
    | handCalc myHand == 11 = True
    | ((toPoints (getMayBeValue dealerUpCard) == 4) || (toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand == 10) = True   
    | ((toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand == 9) = True
    | otherwise = False 

insuranceHand :: Maybe Card -> Maybe String -> Bool
insuranceHand dealerUpCard myMemory
    | getRank (getMayBeValue dealerUpCard) == Ace  && myMemory == Just "FIRST TURN AFTER BIDDING" = True
    | otherwise = False