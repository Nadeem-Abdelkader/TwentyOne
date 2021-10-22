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

    -- Trace - used for debugging the code
    -- | trace ("bid: pid=" ++ show myId ++ " pinfo: " ++ show playersHand ++ " hand: " ++ show myHand) False = undefined 
    -- | trace ("Hand Value: " ++ show (handCalc myHand)) False = undefined 
    -- | trace ("Dealer: " ++ show (getRank (getMayBeValue dealerUpCard))) False = undefined
    
    -- If insuranceHand function returns true, then take insurance action
    | insuranceHand dealerUpCard myMemory = (Insurance (maxInsure maxBid ), "")

    -- If splitHand function returns true, then take split action
    | splitHand dealerUpCard myHand = (Split maxBid, "")

    -- If doubleDownHand function returns true, then take double down action
    -- | doubleDownHand dealerUpCard myHand = (DoubleDown 100, "")

    -- If hitHand function returns true, then take hit action
    | hitHand dealerUpCard myHand = (Hit, "")

    -- If standHand function returns true, then take stand action
    | standHand dealerUpCard myHand = (Stand, "")
    
    -- If none of the conditions were fulfilled, take the stand action
    | otherwise = (Stand, "")

-- This function gets the actual value out of the Maybe context (e.g. Just Card Spade Ace --> Card Space Ace))
getMayBeValue :: Maybe Card  -> Card
getMayBeValue x =
    case x of
          Nothing -> Card Heart Five
          Just val -> val

-- This function creates a suitable bid when its the first round of the game (bidding round)
createSuitableBid :: Hand -> (Action , String)
createSuitableBid currentHand = (Bid maxBid, "FIRST TURN AFTER BIDDING")

-- This function is responsible for deciding whether the player should take the split action or not
-- It will return True if the player should take the split action, and False if the player shouldn't take the split action
splitHand :: Maybe Card -> Hand -> Bool
splitHand dealerUpCard currentHand

    -- Shouldn't take the split action unless exactly 2 cards in hand
    | length currentHand /= 2 = False

    -- | trace ("Hand : " ++ show (currentHand)) False = undefined 
    -- | trace ("1st : " ++ show (getRank (head currentHand) == Ace)) False = undefined 
    -- | trace ("2nd : " ++ show (getRank (head (tail currentHand)) == Ace)) False = undefined 
    -- | trace ("Both : " ++ show (getRank (head currentHand) == Ace && getRank (head (tail currentHand)) == Ace)) False = undefined

    -- If the two cards in hand are Ace's, take the split action (i.e. return True)
    | getRank (head currentHand) == Ace && getRank (head (tail currentHand)) == Ace = True

    -- If the two cards in hand are Eight's, take the split action (i.e. return True)
    | getRank (head currentHand) == Eight && getRank (head (tail currentHand)) == Eight = True

    -- If the two cards in hand are Sevens's and the dealer's up card is worth 2-7, take the split action (i.e. return True)
    | getRank (head currentHand) == Seven && getRank (head (tail currentHand)) == Seven  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7)
    
    -- If the two cards in hand are Two's and the dealer's up card is worth 2-7, take the split action (i.e. return True)
    | getRank (head currentHand) == Two && getRank (head (tail currentHand)) == Two  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7)
    
    -- If the two cards in hand are Three's and the dealer's up card is worth 2-7, take the split action (i.e. return True)
    | getRank (head currentHand) == Three && getRank (head (tail currentHand)) == Three  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 7)

    -- If the two cards in hand are Six's and the dealer's up card is worth 2-6, take the split action (i.e. return True)
    | getRank (head currentHand) == Six && getRank (head (tail currentHand)) == Six  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6)
   
    -- If the two cards in hand are Five's and the dealer's up card is worth 2-9, take the split action (i.e. return True)
    | getRank (head currentHand) == Five && getRank (head (tail currentHand)) == Five  = (toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9)

    -- If the two cards in hand are Four's and the dealer's up card is worth 5-6, take the split action (i.e. return True)
    | getRank (head currentHand) == Four && getRank (head (tail currentHand)) == Four  = (toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6)

    -- If the two cards in hand are Nines's and the dealer's up card is worth not (2-9 BUT NOT 7), take the split action (i.e. return True)
    | getRank (head currentHand) == Nine && getRank (head (tail currentHand)) == Nine  = not ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 9) && (toPoints (getMayBeValue dealerUpCard) /= 7))
    
    -- If none of the conditions were fulfilled, return False (i.e. do not split)
    | otherwise = False

-- This function is responsible for deciding whether the player should take the Hit action or not
-- It will return True if the player should take the Hit action, and False if the player shouldn't take the Hit action
hitHand :: Maybe Card -> Hand -> Bool
hitHand dealerUpCard myHand

    -- IF hand value is equal to Eight, take the hit action (i.e. return True)
    | handCalc myHand == 8 = True
    
    -- If current hand is a Soft 18 and dealer's up card is worth 9 - Ace, take the hit action (i.e. return True)
    | (handCalc myHand == 18) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && (toPoints (getMayBeValue dealerUpCard) >= 9) && (toPoints (getMayBeValue dealerUpCard) <= 11) = True

    -- If current hand is a Soft 17 and dealer's up card is worth 3 - 6, take the hit action (i.e. return True)
    | (handCalc myHand == 17) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && not ((toPoints (getMayBeValue dealerUpCard) >= 3) && (toPoints (getMayBeValue dealerUpCard) <= 6)) = True

    -- If current hand is a Soft 16 and dealer's up card is worth 4 - 6, take the hit action (i.e. return True)
    | (handCalc myHand == 16) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && not ((toPoints (getMayBeValue dealerUpCard) >= 4) && (toPoints (getMayBeValue dealerUpCard) <= 6)) = True

    -- If current hand is a Soft 15 and dealer's up card is worth 4 - 6, take the hit action (i.e. return True)
    | (handCalc myHand == 17) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && not ((toPoints (getMayBeValue dealerUpCard) >= 4) && (toPoints (getMayBeValue dealerUpCard) <= 6)) = True

    -- If current hand is a Soft 14 and dealer's up card is worth not(5 - 6), take the hit action (i.e. return True)
    | (handCalc myHand == 15) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && not ((toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6)) = True

    -- If current hand is a Soft 13 and dealer's up card is worth not(5 - 6), take the hit action (i.e. return True)
    | (handCalc myHand == 15) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && not ((toPoints (getMayBeValue dealerUpCard) >= 5) && (toPoints (getMayBeValue dealerUpCard) <= 6)) = True

    -- If current hand value is equal than or greater than 17 and dealer's up card is an Ace, take the hit action (i.e. return True)
    | getRank (getMayBeValue dealerUpCard) == Ace  && (handCalc myHand <= 17) = True
    
    -- If current hand value is equal to 10 and dealer's up card is a 10, take the hit action (i.e. return True)
    | (toPoints (getMayBeValue dealerUpCard) == 10) && (handCalc myHand == 10) = True

    -- If current hand value is equal to 10-16 and dealer's up card is a 10, take the hit action (i.e. return True)
    | (toPoints (getMayBeValue dealerUpCard) == 10) && ((handCalc myHand >= 12) && (handCalc myHand <= 16)) = True

    -- If current hand value is less than or equal to 9 and dealer's up card is a 7,8,or 9, take the hit action (i.e. return True)
    | ((toPoints (getMayBeValue dealerUpCard) == 7) || (toPoints (getMayBeValue dealerUpCard) == 8) || (toPoints (getMayBeValue dealerUpCard) == 9)) && (handCalc myHand <= 9) = True

    -- If current hand value is equal to 12 - 16 and dealer's up card is a 7,8,or 9, take the hit action (i.e. return True)
    | ((toPoints (getMayBeValue dealerUpCard) == 7) || (toPoints (getMayBeValue dealerUpCard) == 8) || (toPoints (getMayBeValue dealerUpCard) == 9)) && ((handCalc myHand >= 12) && (handCalc myHand <= 16)) = True

    -- If current hand value is less than or equal to 8 and dealer's up card is a 4,5,or 6, take the hit action (i.e. return True)
    | ((toPoints (getMayBeValue dealerUpCard) == 4) || (toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand <= 8) = True

    -- If current hand value is less than or equal to 8 and dealer's up card is a 3, take the hit action (i.e. return True)
    | (toPoints (getMayBeValue dealerUpCard) == 3) && (handCalc myHand <= 8) = True
    
    -- If current hand value is equal to 12 and dealer's up card is a 3, take the hit action (i.e. return True)
    | (toPoints (getMayBeValue dealerUpCard) == 3) && (handCalc myHand == 12) = True

    -- If current hand value is less than or equal to 9 and dealer's up card is a 2, take the hit action (i.e. return True)
    | (toPoints (getMayBeValue dealerUpCard) == 2) && (handCalc myHand <= 9) = True
    
    -- If none of the conditions were fulfilled, return False (i.e. do not hit)
    | otherwise = False

-- This function is responsible for deciding whether the player should take the stand action or not
-- It will return True if the player should take the stand action, and False if the player shouldn't take the stand action
standHand :: Maybe Card -> Hand -> Bool
standHand dealerUpCard myHand

    -- If hand value is equal to or greater than 17, take the stand action (i.e. return True)
    | handCalc myHand >= 17 = True
    
    -- If current hand is a soft 20, take the stand action (i.e. return True)
    | (handCalc myHand == 20) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) = True

    -- If current hand is a soft 19 and the dealer's up card is not 6, take the stand action (i.e. return True)
    | (handCalc myHand == 19) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && (toPoints (getMayBeValue dealerUpCard) /= 6) = True

    -- If current hand is a soft 18 and the dealer's up card is not 2-6 and not 9 - Ace, take the stand action (i.e. return True)
    | (handCalc myHand == 18) && ((getRank (head myHand) == Ace)||(getRank (head (tail myHand)) == Ace)) && not ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6)) && (toPoints (getMayBeValue dealerUpCard) >= 9) = True

    -- If current hand value >= 17 and dealer's up card = 10, take the stand action (i.e. return True)
    | (toPoints (getMayBeValue dealerUpCard) == 10) && (handCalc myHand >= 17) = True

    -- If current hand value >= 17 and dealer's up card = 7 or 8 or 9, take the stand action (i.e. return True)
    | ((toPoints (getMayBeValue dealerUpCard) == 7) || (toPoints (getMayBeValue dealerUpCard) == 8) || (toPoints (getMayBeValue dealerUpCard) == 9)) && (handCalc myHand >= 17) = True
  
    -- If current hand value >= 12 and dealer's up card = 4 or 5 or 6, take the stand action (i.e. return True)
    | ((toPoints (getMayBeValue dealerUpCard) == 4) || (toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand >= 12) = True

    -- If current hand value >= 13 and dealer's up card = 3, take the stand action (i.e. return True)
    | (toPoints (getMayBeValue dealerUpCard) == 3) && (handCalc myHand >= 13) = True 

    -- If current hand value >= 13 and dealer's up card = 2, take the stand action (i.e. return True)
    | (toPoints (getMayBeValue dealerUpCard) == 2) && (handCalc myHand >= 13) = True

    -- If current hand value = 16 and dealer's up card = 2 - 6, take the stand action (i.e. return True)
    | ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6)) && (handCalc myHand == 16) = True
        
    -- If current hand value = 15 and dealer's up card = 2 - 6, take the stand action (i.e. return True)
    | ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6)) && (handCalc myHand == 15) = True
    
    -- If current hand value = 14 and dealer's up card = 2 - 6, take the stand action (i.e. return True)
    | ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6)) && (handCalc myHand == 14) = True
    
    -- If current hand value = 13 and dealer's up card = 2 - 6, take the stand action (i.e. return True)
    | ((toPoints (getMayBeValue dealerUpCard) >= 2) && (toPoints (getMayBeValue dealerUpCard) <= 6)) && (handCalc myHand == 13) = True
    
    -- If current hand value = 12 and dealer's up card = 4 - 6, take the stand action (i.e. return True)
    | ((toPoints (getMayBeValue dealerUpCard) >= 4) && (toPoints (getMayBeValue dealerUpCard) <= 6)) && (handCalc myHand == 12) = True

    -- If none of the conditions were fulfilled, return False (i.e. do not stand)
    | otherwise = False

-- This function is responsible for deciding whether the player should take the double down action or not
-- It will return True if the player should take the double down action, and False if the player shouldn't take the double down action
doubleDownHand :: Maybe Card -> Hand -> Bool
doubleDownHand dealerUpCard myHand

    -- Shouldn't take the double down action unless exactly 2 cards in hand
    | length myHand /= 2 = False

    -- If current hand value = 11, take the double down action (i.e. return True)
    | handCalc myHand == 11 = True

    -- If current hand value = 10 and dealer's up card = 4 - 6, take the double down action (i.e. return True)
    | ((toPoints (getMayBeValue dealerUpCard) == 4) || (toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand == 10) = True   
    
    -- If current hand value = 9 and dealer's up card = 5 - 6, take the double down action (i.e. return True)
    | ((toPoints (getMayBeValue dealerUpCard) == 5) || (toPoints (getMayBeValue dealerUpCard) == 6)) && (handCalc myHand == 9) = True
    
    -- If none of the conditions were fulfilled, return False (i.e. do not double down)
    | otherwise = False 

-- This function is responsible for deciding whether the player should take the insurance action or not
-- It will return True if the player should take the insurance action, and False if the player shouldn't take the insurance action
insuranceHand :: Maybe Card -> Maybe String -> Bool
insuranceHand dealerUpCard myMemory

    -- Only take the insurance action when its the first round after bidding and the dealers up card is an Ace.
    | getRank (getMayBeValue dealerUpCard) == Ace  && myMemory == Just "FIRST TURN AFTER BIDDING" = True
    
    -- If none the conditions wasn't fulfilled, return False (i.e. do not take insurance action)
    | otherwise = False