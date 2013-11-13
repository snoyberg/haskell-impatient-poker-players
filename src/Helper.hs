-- This is a helper module, defining some commonly used code.
-- Other lessons can import this module.
module Helper where

data Suit = Club | Diamond | Heart | Spade
    deriving (Show, Eq, Ord, Enum, Bounded)
data Rank = Two | Three | Four | Five | Six | Seven | Eight
          | Nine | Ten | Jack | Queen | King | Ace
   deriving (Show, Eq, Ord, Enum, Bounded)
data Card = Card Suit Rank
    deriving (Show, Eq, Ord)
deck = do
    suit <- [minBound .. maxBound]
    rank <- [minBound .. maxBound]
    return (Card suit rank)