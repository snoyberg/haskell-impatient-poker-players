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
    | Pair Rank [Rank] -- the rank of the pair, plus the rank of the next three cards (the kickers)
    | TwoPair Rank Rank Rank -- the rank of the high pair, the rank of the low pair, and the last card
    | ThreeOfAKind Rank [Rank]
    | Straight Rank -- high card
    | Flush [Rank] -- need the ranks of all the cards in case there are two flushes
    | FullHouse Rank Rank -- first the triple, then the pair
    | FourOfAKind Rank Rank
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

-- The most complicated analysis is matching cards, since there are so many
-- different possibilities: pair, two pair, three of a kind, four of a kind,
-- and full house. Before we get started, we need a helper function: takeEach.
-- This function itself is pretty complicated, so make sure to read the description
-- and the code carefully. Once you get this, the rest of the code is pretty easy.
--
-- Suppose we have five cards. What we're going to want to do is analyze each
-- of the five cards against the other four cards, one at a time. This function
-- allows this, by creating a list of those five different combinations. We can
-- think of this like keeping two stacks of cards: the front and the back.
-- We take one card off the back, create a result of all the other front and back
-- cards, and then put that card on the front stack and continue. Let's see how this
-- works.
takeEach xs =
    -- We start off with all cards on the back stack and an empty front stack.
    go [] xs
  where
    -- When the back stack is empty, we're all done.
    go front [] = []
    -- Take one card off the back, create a pair of that card with all the front
    -- and back stack cards, and then put the card on the front stack and keep going.
    go front (x:back) = (x, front ++ back) : go (x:front) back

-- This is a simple helper function we'll be using below.
isRank rank1 (Card _ rank2) = rank1 == rank2

matches cards = do
    -- We'll go through each of the five cards, one at a time.
    (Card _ rank, rest) <- takeEach cards
    -- Once we know the rank of the first card, we find all of
    -- the other cards that match it. To do this, we use the
    -- partition function, which splits a list into cards that
    -- match and cards that don't match.
    case partition (isRank rank) rest of
        -- The first list is empty, meaning there were no matching cards.
        ([], _) -> []
        -- There's exactly one matching card, so we know we have a pair.
        -- That leaves three options: one pair, two pair, or a full house.
        -- We need to determine which case we're dealing with.
        ([_], rest2) -> do
            -- We need to look at each of the three remaining cards, one
            -- at a time, the same as we did before.
            (Card _ rank2, rest3) <- takeEach rest2
            -- Again, let's find all matches.
            case partition (isRank rank2) rest3 of
                -- No cards match, so we have one pair.
                ([], _) -> return (Pair rank (cardRanks rest2))
                -- There was exactly one match, so we have two pair.
                ([_], [Card _ kicker]) ->
                    -- We need to make sure that the higher rank appears
                    -- first, to make sure that comparison give higher weight
                    -- to the higher pair.
                    if rank > rank2
                        then return (TwoPair rank rank2 kicker)
                        else return (TwoPair rank2 rank kicker)
                -- There were two matches, so we have a full house.
                ([_, _], []) -> return (FullHouse rank2 rank)
        -- There are two matches, so we have at least a three of a kind. Now
        -- we need to test if we also have a full house.
        ([_, _], rest) -> do
            let [Card _ kicker1, Card _ kicker2] = rest
            if kicker1 == kicker2
                then return (FullHouse rank kicker1)
                else return (ThreeOfAKind rank (cardRanks rest))
        -- Three matches means we have a four of a kind.
        ([_, _, _], [Card _ kicker]) -> return (FourOfAKind rank kicker)

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