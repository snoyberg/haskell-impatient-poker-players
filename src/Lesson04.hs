module Lesson04 where

import Helper
import System.Random.Shuffle

-- Let's make our game a little bit more interesting: we're going to
-- add in some money. We'll have a limited form of betting, by giving
-- the player some money at the start of the game, and letting him bet
-- against the house. We haven't yet written the rules for determining
-- who won the game, so we'll have to trust the user for now. Don't
-- worry, we'll fix that later. (We'll also eventually let you play
-- against other human players too.)

-- What we want is to have some variable that keeps track of how much
-- money the player has. But as I've mentioned previously, there's no
-- way to change a value. The way we emulate changing values is by
-- having a function that takes input representing the money the
-- player has, and changing the value we input over time.

-- So let's get started with a function that let's a player play one
-- hand of poker, and returns how much the player won or lost in
-- the bet.
playHand = do
    shuffled <- shuffleM deck
    let (player, rest) = splitAt 5 shuffled
        dealer = take 5 rest
    putStrLn ("Your hand is: " ++ show player)
    putStrLn "Enter your wager"
    bet <- readLn
    putStrLn ("The dealer has: " ++ show dealer)
    putStrLn "Did you win? (y/n)"
    answer <- getLine
    if answer == "y"
        then return bet     -- you won
        else return (- bet) -- you lost

-- Now that we know how to play a single hand, let's write our "main loop."
-- This is the function that our game will live in. We call it a loop because
-- it will run itself multiple times. We'll see how this is done at the end
-- of the function.
--
-- The function takes one argument: the number of chips the player has when
-- starting this round.
playGame chips = do
    putStrLn ("Your current chip count: " ++ show chips)
    chipChange <- playHand
    let newChips = chips + chipChange
    if newChips > 0
        -- We have some chips left, so let's play the game again based on the
        -- new chip count.
        then playGame newChips
        else putStrLn "You're out of chips. Goodbye!"

-- Finally, we still need to have a main function. This function will start our
-- game off with an initial number of chips (let's say 10).
main = playGame 10

{-

Exercises:

1. The user is currently allowed to entered negative bets, which shouldn't be
   allowed. Change it so that, if the user enters a negative bet, he/she is
   asked to enter a non-negative number. Hint: to do this, you'll need to replace
   readLn in playHand with a separate function that you're going to write.

2. The user is allowed to enter bets larger than his/her total number of chips.
   Make a similar modification as done in the previous exercise to prevent this.
   Note that you'll need to modify the playHand function so it takes the total
   number of chips as an argument.

Behind the scenes:

FIXME to be written, should talk about recursion

-}