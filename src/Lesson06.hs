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

cardRank (Card _ rank) = rank
dsort = reverse . sort
cardRanks = dsort . map cardRank

-- Every hand matches high card, we just sort the cards in descending order.
highCard cards = return (HighCard (cardRanks cards))

takeEach xs =
    go [] xs
  where
    go front (x:back) = (x, front ++ back) : go (x:front) back
    go front [] = []

isRank rank1 (Card _ rank2) = rank1 == rank2

matches cards = do
    (Card _ rank, rest) <- takeEach cards
    case partition (isRank rank) rest of
        ([], _) -> fail "No matches"
        ([_], rest2) -> do
            (Card _ rank2, rest3) <- takeEach rest2
            case partition (isRank rank2) rest3 of
                ([], _) -> return (Pair rank (cardRanks rest2))
                ([_], [Card _ kicker]) ->
                    if rank > rank2
                        then return (TwoPair rank rank2 kicker)
                        else return (TwoPair rank2 rank kicker)
                ([_, _], []) -> return (FullHouse rank2 rank)
        ([_, _], rest) -> do
            let [Card _ kicker1, Card _ kicker2] = rest
            if kicker1 == kicker2
                then return (FullHouse rank kicker1)
                else return (ThreeOfAKind rank (cardRanks rest))
        ([_, _, _], [Card _ kicker]) -> return (FourOfAKind rank kicker)

isSuit suit1 (Card suit2 _) = suit1 == suit2

isFlush (Card suit _:rest) = all (isSuit suit) rest

flush cards =
    if isFlush cards
        then return (Flush (cardRanks cards))
        else fail "Not a flush"

straight cards = do
    -- Special rule: if we have both an ace and a two in the hand,
    -- then we'll remove the ace entirely.
    let sorted =
            case sort (map cardRank cards) of
                [Two, x, y, z, Ace] -> [Two, x, y, z]
                x -> x
    highest <- highestFromRun sorted
    if isFlush cards
        then return (StraightFlush highest)
        else return (Straight highest)

highestFromRun [x] = return x
highestFromRun (x:y:rest) =
    if x < Ace && succ x == y
        then highestFromRun (y:rest)
        else fail "Not a run"

pokerHand cards = maximum (concat
    [ highCard cards
    , matches cards
    , flush cards
    , straight cards
    ])

main = do
    shuffled <- shuffleM deck
    let (cards1, rest) = splitAt 5 shuffled
        cards2 = take 5 rest
        hand1 = pokerHand cards1
        hand2 = pokerHand cards2
    putStrLn ("Player 1: " ++ prettyHand cards1 ++ ", " ++ show hand1)
    putStrLn ("Player 2: " ++ prettyHand cards2 ++ ", " ++ show hand2)
    case compare hand1 hand2 of
        LT -> putStrLn "Player 2 wins"
        EQ -> putStrLn "Tie"
        GT -> putStrLn "Player 1 wins"

    putStrLn "Player again? y/n"
    answer <- getLine
    if answer == "y"
        then main
        else putStrLn "Goodbye!"