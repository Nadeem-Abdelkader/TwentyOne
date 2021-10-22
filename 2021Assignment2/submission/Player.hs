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

    -- TRACE FOR DEBUGGING CODE
    -- | trace ("bid: pid=" ++ show myId ++ " pinfo: " ++ show playersHand ++ " hand: " ++ show myHand) False = undefined 
    -- | trace ("Hand Value: " ++ show (handCalc myHand)) False = undefined 
    -- | trace ("Dealer: " ++ show (getRank (getMayBeValue dealerUpCard))) False = undefined
    
    -- IF insuranceHand FUNCTION RETURNS TRUE THEN TAKE INSURANCE ACTION
    | insuranceHand dealerUpCard myMemory = (Insurance (maxInsure maxBid ), "")

    -- IF splitHand FUNCTION RETURNS TRUE THEN TAKE SPLIT ACTION
    | splitHand dealerUpCard myHand = (Split maxBid, "")

    -- IF doubleDownHand FUNCTION RETURNS TRUE THEN TAKE DOUBLE DOWN ACTION
    -- | doubleDownHand dealerUpCard myHand = (DoubleDown 100, "")

    -- IF hitHand FUNCTION RETURNS TRUE THEN TAKE HIT ACTION
    | hitHand dealerUpCard myHand = (Hit, "")

    -- IF standHand FUNCTION RETURNS TRUE THEN TAKE STAND ACTION
    | standHand dealerUpCard myHand = (Stand, "")
    
    -- IF NONE OF THE CONDITIONS WERE FULFILLED TAKE THE STAND ACTION
    | otherwise = (Stand, "")

-- THIS FUNCTION GETS THE ACTUAL VALUE OUT OF THE MAYBE CONTEXT (E.G. Just Card Spade Ace --> Card Space Ace)
getMayBeValue :: Maybe Card  -> Card
getMayBeValue x =
    case x of
          Nothing -> Card Heart Five
          Just val -> val

-- THIS FUNCTION CREATES A SUITABLE BID WHEN ITS THE FIRST ROUND (BIDDING ROUND)
createSuitableBid :: Hand -> (Action , String)
createSuitableBid currentHand = (Bid maxBid, "FIRST TURN AFTER BIDDING")

-- THIS FUNCTION IS RESPONSIBLE FOR DECIDING WHETHER THE PLAYER SHOULD TAKE THE SPLIT ACTION OR NOT
-- IT WILL RETURN TRUE IF THE PLAYER SHOULD SPLIT, FALSE IF HE SHOULDN'T SPLIT
splitHand :: Maybe Card -> Hand -> Bool
splitHand dealerUpCard currentHand

    -- SHOULDN'T TAKE SPLIT ACTION UNLESS EXACTLY 2 CARDS IN HAND
    | length currentHand /= 2 = False

    -- | trace ("Hand : " ++ show (currentHand)) False = undefined 
    -- | trace ("1st : " ++ show (getRank (head currentHand) == Ace)) False = undefined 
    -- | trace ("2nd : " ++ show (getRank (head (tail currentHand)) == Ace)) False = undefined 
    -- | trace ("Both : " ++ show (getRank (head currentHand) == Ace && getRank (head (tail currentHand)) == Ace)) False = undefined

    -- IF THE TWO CARD'S IN HAND ARE ACES, TAKE THE SPLIT ACTION (i.e. RETURN TRUE)
    | getRank (head currentHand) == Ace && getRank (head (tail currentHand)) == Ace = True

    -- IF THE TWO CARD'S IN HAND ARE EIGHTS, TAKE THE SPLIT ACTION (i.e. RETURN TRUE)
    | getRank (head currentHand) == Eight && getRank (head (tail currentHand)) == Eight = True

    -- IF THE TWO CARD'S IN HAND ARE SEVENS AND DEALER'S UP CARD IS WORTH 2-7, TAKE THE SPLIT ACTION (i.e. RETURN TRUE)
    | getRank (head currentHand) == Seven && getRank (head (tail currentHand)) == Seven  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7)
    
    -- IF THE TWO CARD'S IN HAND ARE TWOS AND DEALER'S UP CARD IS WORTH 2-7, TAKE THE SPLIT ACTION (i.e. RETURN TRUE)
    | getRank (head currentHand) == Two && getRank (head (tail currentHand)) == Two  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7)
    
    -- IF THE TWO CARD'S IN HAND ARE THREES AND DEALER'S UP CARD IS WORTH 2-7, TAKE THE SPLIT ACTION (i.e. RETURN TRUE)
    | getRank (head currentHand) == Three && getRank (head (tail currentHand)) == Three  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7)

    -- IF THE TWO CARD'S IN HAND ARE SIXES AND DEALER'S UP CARD IS WORTH 2-6, TAKE THE SPLIT ACTION (i.e. RETURN TRUE)
    | getRank (head currentHand) == Six && getRank (head (tail currentHand)) == Six  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6)
   
    -- IF THE TWO CARD'S IN HAND ARE FIVES AND DEALER'S UP CARD IS WORTH 2-9, TAKE THE SPLIT ACTION (i.e. RETURN TRUE)
    | getRank (head currentHand) == Five && getRank (head (tail currentHand)) == Five  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9)

    -- IF THE TWO CARD'S IN HAND ARE FOURS AND DEALER'S UP CARD IS WORTH 5-6, TAKE THE SPLIT ACTION (i.e. RETURN TRUE)
    | getRank (head currentHand) == Four && getRank (head (tail currentHand)) == Four  = (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6)

    -- IF THE TWO CARD'S IN HAND ARE NINES AND DEALER'S UP CARD IS WORTH NOT (2-9 BUT NOT 7), TAKE THE SPLIT ACTION (i.e. RETURN TRUE)
    | getRank (head currentHand) == Nine && getRank (head (tail currentHand)) == Nine  = not ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && (toPoints (getMayBeValue dealerUpCard) /= 7))
    
    | otherwise = False

-- THIS FUNCTION IS RESPONSIBLE FOR DECIDING WHETHER THE PLAYER SHOULD TAKE THE HIT ACTION OR NOT
-- IT WILL RETURN TRUE IF THE PLAYER SHOULD HIT, FALSE IF HE SHOULDN'T HIT
hitHand :: Maybe Card -> Hand -> Bool
hitHand dealerUpCard myHand

    -- IF HAND VALUE IS EQUAL TO EIGHT, TAKE THE HIT ACTION
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

-- THIS FUNCTION IS RESPONSIBLE FOR DECIDING WHETHER THE PLAYER SHOULD TAKE THE SPLIT ACTION OR NOT
-- IT WILL RETURN TRUE IF THE PLAYER SHOULD SPLIT, FALSE IF HE SHOULDN'T SPLIT
standHand :: Maybe Card -> Hand -> Bool
standHand dealerUpCard myHand

    -- IF HAND VALUE EQUAL THAN OR GREATER THAN 17, TAKE THE STAND ACTION
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

-- THIS FUNCTION IS RESPONSIBLE FOR DECIDING WHETHER THE PLAYER SHOULD TAKE THE DOUBLE DOWN ACTION OR NOT
-- IT WILL RETURN TRUE IF THE PLAYER SHOULD DOUBLE DOWN, FALSE IF HE SHOULDN'T DOUBLE DOWN
doubleDownHand :: Maybe Card -> Hand -> Bool
doubleDownHand dealerUpCard myHand

    -- SHOULD'T DOUBLE DOWN UNLESS YOU HAVE EXACTLY 2 CARDS IN HAND
    | length myHand /= 2 = False

    -- | (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && handCalc myHand == 10 = True  
    -- | (toPoints (getMayBeValue dealerUpCard) >= 3) && (toPoints (getMayBeValue dealerUpCard) <= 6) && handCalc myHand == 9 = True  
    | handCalc myHand == 11 = True
    | ((toPoints (getMayBeValue dealerUpCard) == 4) || (toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand == 10) = True   
    | ((toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand == 9) = True
    | otherwise = False 

-- THIS FUNCTION IS RESPONSIBLE FOR DECIDING WHETHER THE PLAYER SHOULD TAKE THE INSURANCE ACTION OR NOT
-- IT WILL RETURN TRUE IF THE PLAYER SHOULD INSURANCE, FALSE IF HE SHOULDN'T INSURANCE
insuranceHand :: Maybe Card -> Maybe String -> Bool
insuranceHand dealerUpCard myMemory
    -- ONLY TAKE INSURANCE ACTION, WHEN ITS THE FIRST ROUND AFTER BIDDING AND THE DEALERS UP CARD IS AN ACE
    | getRank (getMayBeValue dealerUpCard) == Ace  && myMemory == Just "FIRST TURN AFTER BIDDING" = True
    
    | otherwise = False