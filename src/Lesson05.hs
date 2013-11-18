module Lesson05 where

-- Today's lesson is all about *style*. I bet you're tired of running these
-- little apps in the bottom of the IDE. Wouldn't it be cool to have a fully
-- powered website instead? Also, the display of a hand is pretty verbose.
-- Let's make our hand display prettier.

import Helper
import Helper.Multiplayer -- New import- it's a helper module for this tutorial.
import System.Random.Shuffle
import Data.List -- also a new import, needed for intercalate below
import Safe -- needed for readMay

-- Alright, prettier cards first. Instead of saying "Card Heart Jack", let's say
-- "JH." And so and and so forth. This is just a bunch of repetitive code really:
prettySuit Club = "C"
prettySuit Diamond = "D"
prettySuit Heart = "H"
prettySuit Spade = "S"

prettyRank Two = "2"
prettyRank Three = "3"
prettyRank Four = "4"
prettyRank Five = "5"
prettyRank Six = "6"
prettyRank Seven = "7"
prettyRank Eight = "8"
prettyRank Nine = "9"
prettyRank Ten = "10"
prettyRank Jack = "J"
prettyRank Queen = "Q"
prettyRank King = "K"
prettyRank Ace = "A"

prettyCard (Card suit rank) = prettyRank rank ++ prettySuit suit

-- This part is interesting. A hand is a list of cards. We want to do two things
-- to this list:
--
-- 1. Convert each card to its pretty form using prettyCard.
--
-- 2. Combine the five individual Strings together into one big String, with commas
--    between each one.
--
-- The function map is perfect for handling the first point, and the scarily-named
-- function intercalate does the second.
prettyHand = intercalate ", " . map prettyCard

-- And now to make this web-based. The Helper.Multiplayer module is something
-- I wrote just for this tutorial. It makes it easy to have multiplayer, text
-- based web games. For now, we'll be sticking to single player, but we'll come
-- back to more impressive things later.
--
-- Let's take our code from the previous tutorial and tweak it to:
--
-- 1. Use our prettyHand function above.
--
-- 2. Instead of putStrLn, getLine, and readLn, use the special functions
--    askPlayer, tellPlayer, and getPlayers, which Helper.Multiplayer provides.
--
-- We're also going to make our code a little bit more robust, by checking for
-- invalid input. We'll start that off with a better way of getting bets.
getWager chips = do
    [player] <- getPlayers
    wagerString <- askPlayer player "Please enter your wager"
    -- We'll use readMay to try to read the wager string as an
    -- integer.
    case readMay wagerString of
        Nothing -> do
            tellPlayer player "Your wager must be a valid integer."
            getWager chips
        Just wager
            | wager < 0 -> do
                tellPlayer player "Your wager must not be negative."
                getWager chips
            | wager > chips -> do
                tellPlayer player "You cannot bet more than your total chips."
                getWager chips
            | otherwise -> do
                tellPlayer player ("You have wagered: " ++ show wager)
                return wager

playHand chips = do
    [player] <- getPlayers
    shuffled <- shuffleM deck
    let (hand, rest) = splitAt 5 shuffled
        dealer = take 5 rest
    tellPlayer player ("Your hand is: " ++ prettyHand hand)
    bet <- getWager chips
    tellPlayer player ("The dealer has: " ++ prettyHand dealer)
    answer <- askPlayer player "Did you win? (y/n)"
    if answer == "y"
        then return bet     -- you won
        else return (- bet) -- you lost

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

-- And now our main function needs to use the special playMultiplayerGame provided
-- by Helper.Multiplayer. We need to tell it a name for the game and how many players
-- per game.
main = playMultiplayerGame "five card stud against the house" 1 (playGame 10)
{-

Exercises:

Behind the scenes:

FIXME: function composition, higher order functions, case statements, guards

-}