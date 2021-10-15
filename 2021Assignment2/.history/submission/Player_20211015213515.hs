-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game
import Data.Maybe

-- You can add more imports if you need them

getMayBeValue :: Maybe Card -> Card
getMayBeValue x =
    case x of
          Nothing -> 0
          Just val -> val

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard dealerUpCard playersPoints playersHand myId myMemory myHand
    | isNothing dealerUpCard = (Bid 100, "")
    | getRank (getMayBeValue dealerUpCard) == Ace = (Split 100,"")
    -- | = (Hit, "")
    -- | = (Stand, "")
    -- | = (DoubleDown Points, "")
    -- | = (Split Points, "")
    -- | = (Insurance Points, "")
    | otherwise = (Stand ,"")

-- Actions = Bid Points | Hit | Stand | DoubleDown Points | Split Points | Insurance Points
