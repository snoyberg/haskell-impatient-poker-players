module Lesson07 where

-- We're getting close to some actually useful code. In lesson 5, we ended up
-- with a single-player game of poker against the house. In lesson 6, we wrote
-- some functions to calculate winning hands. This lesson is a relatively simple
-- combination of those two lessons.
--
-- Most of the code here is copied directly from lesson 5, I'll add comments for
-- the changes.

import Helper
import Helper.Multiplayer
import Helper.Pretty
import Helper.Winning
import System.Random.Shuffle
import Data.List
import Safe
import Lesson05 (getWager)

playHand chips = do
    [player] <- getPlayers
    shuffled <- shuffleM deck
    let (playerCards, rest) = splitAt 5 shuffled
        playerHand = pokerHand playerCards
        dealerCards = take 5 rest
        dealerHand = pokerHand dealerCards
    tellPlayer player ("Your hand is: " ++ prettyHand playerCards ++ ", " ++ show playerHand)
    bet <- getWager chips
    tellPlayer player ("The dealer has: " ++ prettyHand dealerCards ++ ", " ++ show dealerHand)
    case compare playerHand dealerHand of
        LT -> do
            tellPlayer player "You lost"
            return (-bet)
        EQ -> do
            tellPlayer player "Tie"
            return 0
        GT -> do
            tellPlayer player "You win!"
            return bet

playGame chips = do
    [player] <- getPlayers
    tellPlayer player ("Your current chip count: " ++ show chips)
    chipChange <- playHand chips
    let newChips = chips + chipChange
    if newChips > 0
        -- We have some chips left, so let's play the game again based on the
        -- new chip count.
        then playGame newChips
        else tellPlayer player "You're out of chips. Goodbye!"

main = playMultiplayerGame "five card stud against the house" 1 (playGame 10)