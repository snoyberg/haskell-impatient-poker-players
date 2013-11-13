module Lesson03 where

import Helper
import System.Random.Shuffle

-- Let's make things a little more complicated. Instead of hard-coding
-- two players in our game, let's allow the user to specify how many
-- players to allow. In order to make this work, we're going to need
-- to define a helper function.
--
-- This function is going to take a number telling us how many players
-- there are, and the deck. Fortunately, defining a function that takes
-- two arguments is easy:
deal numberOfPlayers shuffled =
    -- We're going to use a technique called recursion, which means our
    -- function will end up calling itself. In order to make that work,
    -- we need to start off with the "base case," which is when there are
    -- no players. In that case, we want to tell the user that all hands
    -- have been dealt:
    if numberOfPlayers == 0
        then putStrLn "All hands dealt"
        -- If numberOfPlayers is not 0, then...
        else do
            -- Again, we'll split up our deck into the current hand and
            -- the rest of the deck using splitAt:
            let (hand, rest) = splitAt 5 shuffled

            -- Now we can print the current hand:
            print hand

            -- And now the fun part: we need to deal the rest of the players.
            -- In order to make this work, we need to call deal again, but
            -- this time with one less player, and with the new list of
            -- cards.
            deal (numberOfPlayers - 1) rest

-- Now that we've written our helper function, main should be easy:
main = do
    putStrLn "How many players?"
    numberOfPlayers <- readLn
    shuffled <- shuffleM deck
    deal numberOfPlayers shuffled

{-

Exercises:

1. What happens if you enter a negative number of players? Can you think of a way
   to make the program behave better?

2. What happens if you enter an invalid number?

3. Can you make the program ask to deal another game after the first one finishes?
   Hint: what you want it to do is, after it deals, start running main again.

4. Advanced: in the previous example, we printed the player number before each hand.
   Can you do that here? Hint: you'll want to pass in an extra argument to deal
   to give the player number, and add 1 to it each time you call deal again.
   You'll also need to give an initial value when you call deal in main.

Behind the scenes:

The code above used an if/then/else statement, which is pretty common in many programming
languages. While this works in Haskell, it's not what most Haskellers would consider
pretty. Usually, Haskell favors something called "pattern matching." The easiest way to
explain that technique is to rewrite deal using it:

-}

deal2 0 shuffled = putStrLn "All hands dealt"
deal2 numberOfPlayers shuffled = do
    let (hand, rest) = splitAt 5 shuffled
    print hand
    deal (numberOfPlayers - 1) rest

{-

The way pattern matching works is that Haskell tries to match the parameters
against the patterns, one at a time. If the number of players is 0, then
the first pattern (or first *clause*) matches, and we print "All hands dealt."
Otherwise, it moves onto the next clause. Both numberOfPlayers and shuffled are
variable names, and therefore count 

-}