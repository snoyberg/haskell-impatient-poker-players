module Lesson06 where

import Helper
import Helper.Pretty -- contains the pretty functions we defined in the previous lesson
import Data.List
import System.Random.Shuffle

-- In this lesson, we're going to take a step back from cool
-- interactions and web interfaces, and instead focus on
-- something a bit more fundamental: calculating the winning
-- hand in a game. When calculating the winner, each player's
-- set of five cards needs to be converted into a poker hand,
-- e.g. a seven-high straight, a flush, etc.
--
-- We're going to create a datatype to represent each possible
-- poker hand. We'll structure it from lowest to highest hand,
-- so that comparing two different hands tells us which is the
-- winning hand.
--
-- Note that we're going to keep exactly enough information to
-- determine the winner. In the case of a pair, for example,
-- we need to know both the rank of the pair itself, plus the rank
-- of the three remaining hands, in descending order, in case two
-- players have matching pairs. We don't, however, keep track of
-- suits, since suits are irrelevant for winners.
data PokerHand
    = HighCard [Rank]
    | Pair Rank Rank Rank Rank -- the rank of the pair, plus the rank of the next three cards (the kickers)
    | TwoPair Rank Rank Rank -- the rank of the high pair, the rank of the low pair, and the last card
    | ThreeOfAKind Rank Rank Rank -- the rank of the three matching cards, plus the two kickers
    | Straight Rank -- high card
    | Flush [Rank] -- need the ranks of all the cards in case there are two flushes
    | FullHouse Rank Rank -- first the triple, then the pair
    | FourOfAKind Rank Rank -- the four of the kind and the kicker
    | StraightFlush Rank
    deriving (Show, Eq, Ord)

-- Now we're going to write some functions for checking for each of
-- these kinds of hands given a player's five cards. What's important
-- is that we're only checking for the presence of this type of hand,
-- not checking if there's a better hand as well. At the end, we'll
-- take all of the hands the player has and choose the best one.
--
-- Each function below will return a list of poker hands that matches
-- the card given. If no matches are found, then we'll return an empty
-- list.

-- First, some helper functions. cardRank gets us the rank of a single
-- card. dsort does a descending sort, by sorting into ascending order
-- and then reversing. Finally, cardRanks combines these two functions
-- to get the rank of a hand in descending order. We'll use this function
-- quite a bit below.
cardRank (Card _ rank) = rank
dsort = reverse . sort
cardRanks = dsort . map cardRank

-- Every hand matches high card, we just sort the cards in descending order.
highCard cards = return (HighCard (cardRanks cards))

-- The next easiest thing to test for is a flush. We need to test for flushes
-- twice: once for flushes themselves, and once for straight-flushes. Therefore,
-- we'll define a helper function that can be used in both places. We start off
-- by pattern matching the list of cards to get the first card's suit. Then we
-- check if the suit of all the other cards is the same suit.
isSuit suit1 (Card suit2 _) = suit1 == suit2
isFlush (Card suit _:rest) = all (isSuit suit) rest

-- Once we have that helper functions, writing a flush checking function is
-- pretty easy.
flush cards =
    if isFlush cards
        then return (Flush (cardRanks cards))
        else [] -- empty list, meaning no match

-- The next easiest thing to check for is a straight. For the most part, this
-- is easy: we sort the cards in ascending order, and then check if each card
-- is one greater than the card before it. There are two gotchas to be aware of:
--
-- 1. There is no card greater than an Ace, so we can't make a comparison against
--    the rank greater than an Ace.
--
-- 2. Ace can work as both high and low card. We're going to use a simple trick
--    to work around this complication: if the sorted hand has a two as the lowest
--    card and an ace as the highest card, we'll just remove the high ace from
--    our comparison. Play around with a few examples in your head to make sure that's
--    a safe trick to employ.
--
-- We'll start with a helper function that will get the highest rank used in a run
-- of cards, assuming the cards do in fact make a run.
highestFromRun (x:y:rest) =
    if x < Ace && succ x == y
        then highestFromRun (y:rest)
        else [] -- not a run, return an empty list
-- If there's only one card left, then we had a run. Return the card.
highestFromRun [x] = return x

straight cards = do
    let sorted =
            case sort (map cardRank cards) of
                [Two, x, y, z, Ace] -> [Two, x, y, z]
                x -> x
    highest <- highestFromRun sorted
    if isFlush cards
        then return (StraightFlush highest)
        else return (Straight highest)

-- Finally, we want to search for matching cards. There are a number of
-- different possibilities: pair, two pair, three of a kind, four of a kind,
-- and full house. Fortunately, we can make this analysis pretty easy using
-- pattern matching. Let me describe the approach at a high level:
--
-- 1. Sort the five cards into descending ranks using cardRanks.
--
-- 2. We want to group all matching ranks into sublists. Haskell provides a function
--    to do just that: the group function. Supposing we had a hand with the cards
--    8C, 8D, KS, AC, 9D, the result of this step would be:
--
--        [[Ace], [King], [Nine], [Eight, Eight]]
--
-- 3. Sort the sublists in descending length order, so that the largest collection
--    of matches appear first. If we apply that to the example above, we get:
--
--        [[Eight, Eight], [Ace], [King], [Nine]]
--
-- 4. Then it's simply a matter of matching all of the different possible patterns
--    to determine the type of hand the player has.
--
-- Let's start by implementing the helper function we'll need for step (3).
descendingLengths =
    -- sortBy uses some comparison function to determine the order for two
    -- elements in the list.
    sortBy compareLengths
  where
    -- We want to compare two lists so that the longer list comes first.
    -- To do that, we compare the lengths *in reverse*, since compare would
    -- normally make the smaller number appear earlier.
    compareLengths x y = compare (length y) (length x)

matches cards =
    -- We want to pattern match after apply step 1 (cardRanks), step 2 (group),
    -- and step 3 (descendingLengths).
    case descendingLengths (group (cardRanks cards)) of
        -- If the result is five one-element lists, it means that
        -- there are no matching cards at all. In that case, we
        -- have no results from this function.
        [[_], [_], [_], [_], [_]] -> []

        -- We have one pair and three non-matches.
        [[pair, _], [kicker1], [kicker2], [kicker3]] -> return (Pair pair kicker1 kicker2 kicker3)

        -- Two pairs with one non-match.
        [[pair1, _], [pair2, _], [kicker]] -> return (TwoPair pair1 pair2 kicker)

        -- A three of a kind and two non-matches.
        [[three, _, _], [kicker1], [kicker2]] -> return (ThreeOfAKind three kicker1 kicker2)

        -- A three of a kind and a pair, also known as a full house.
        [[three, _, _], [pair, _]] -> return (FullHouse three pair)

        -- A four of a kind and one non-match.
        [[four, _, _, _], [kicker]] -> return (FourOfAKind four kicker)

        -- Spend some time thinking if you can come up with any
        -- combination which isn't matched by the patterns above.

-- Now that we have all of our helper functions, we want to use them to
-- get a list of all the possible hands. Since each of our functions returns
-- a list of hands, we use the concat function, we combines a bunch of lists
-- into a single list.
allPokerHands cards = concat
    [ highCard cards
    , matches cards
    , flush cards
    , straight cards
    ]

-- And all we really care about is the highest of all the hands. We can
-- use the maximum function to get the greatest value from a list.
pokerHand = maximum . allPokerHands

-- After all of that hard work, the main function should be easy. We'll
-- just generate two hands and determine the winner.
main = do
    shuffled <- shuffleM deck
    let (cards1, rest) = splitAt 5 shuffled
        cards2 = take 5 rest
        hand1 = pokerHand cards1
        hand2 = pokerHand cards2
    putStrLn ("Player 1: " ++ prettyHand cards1 ++ ", " ++ show hand1)
    putStrLn ("Player 2: " ++ prettyHand cards2 ++ ", " ++ show hand2)

    -- compare returns one of three values: LT means less than, EQ means
    -- equal, and GT means greater than.
    case compare hand1 hand2 of
        LT -> putStrLn "Player 2 wins"
        EQ -> putStrLn "Tie"
        GT -> putStrLn "Player 1 wins"

    putStrLn "Play again? y/n"
    answer <- getLine
    if answer == "y"
        then main
        else putStrLn "Goodbye!"