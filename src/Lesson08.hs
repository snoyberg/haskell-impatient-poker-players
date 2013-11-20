module Lesson08 where

-- Now let's have some real fun: a two player, online five card stud game,
-- with a full betting system. The betting system is actually the biggest
-- addition versus what we've done previously, so most of our attention
-- will be focused on that. Most of the other code will be very similar
-- to what we had in lesson 7.

import Helper
import Helper.Multiplayer
import Helper.Pretty
import Helper.Winning
import System.Random.Shuffle
import Data.List
import Safe

-- We're going to want to keep track of multiple information per player.
-- A common way to do that is to create a record data type, where each
-- piece of data has its own name. We'll want to have the player and
-- how much money he/she has.
data PokerPlayer = PokerPlayer
    { player :: Player
    , chips :: Int
    , cards :: [Card]
    , hand :: PokerHand
    }

data Action = Call | Raise Int | Fold

askAction p allowedRaise = do
    str <- askPlayer (player p) "call, raise, or fold?"
    case str of
        "call" -> return Call
        "raise" ->  askRaise p allowedRaise
        "fold" -> return Fold
        _ -> do
            tellPlayer (player p) "That was not a valid answer"
            askAction p allowedRaise

askRaise p allowedRaise = do
    str <- askPlayer (player p) ("Enter amount to raise, up to " ++ show allowedRaise)
    case readMay str of
        Nothing -> do
            tellPlayer (player p) "That was an invalid raise amount"
            askRaise p allowedRaise
        Just amount
            | amount < 0 -> do
                tellPlayer (player p) "You cannot raise by a negative value"
                askRaise p allowedRaise
            | otherwise -> return (Raise amount)

wager p1 p2 pot owed = do
    tellAllPlayers $ show (player p1) ++ " has " ++ show (chips p1) ++ " chips"
    tellAllPlayers $ show (player p2) ++ " has " ++ show (chips p2) ++ " chips"
    tellAllPlayers $ "The pot currently has " ++ show pot ++ " chips"
    tellAllPlayers $ "Betting is to " ++ show (player p1) ++ ", who owes " ++ show owed
    let allowedRaise = min (chips p2) (chips p1 - owed)
    action <- askAction p1 allowedRaise
    case action of
        Call -> do
            let p1' = p1 { chips = chips p1 - owed }
                pot' = pot + owed
            finishHand p1' p2 pot'
        Fold -> do
            tellAllPlayers $ show (player p1) ++ " folds"
            startGame (player p1) (chips p1) (player p2) (chips p2 + pot)
        Raise raise -> do
            let p1' = p1 { chips = chips p1 - owed - raise }
            wager p2 p1' (pot + owed + raise) raise

finishHand p1 p2 pot = do
    tellAllPlayers ("All bets are in, the pot is at: " ++ show pot)
    tellAllPlayers (show (player p1) ++ " has " ++ prettyHand (cards p1) ++ ", " ++ show (hand p1))
    tellAllPlayers (show (player p2) ++ " has " ++ prettyHand (cards p2) ++ ", " ++ show (hand p2))
    (winnings1, winnings2) <-
        case compare (hand p1) (hand p2) of
            LT -> do
                tellAllPlayers (show (player p2) ++ " wins!")
                return (0, pot)
            EQ -> do
                tellAllPlayers "Tied game"
                let winnings1 = pot `div` 2
                    winnings2 = pot - winnings1
                return (winnings1, winnings2)
            GT -> do
                tellAllPlayers (show (player p1) ++ " wins!")
                return (pot, 0)
    startGame (player p1) (chips p1 + winnings1) (player p2) (chips p2 + winnings2)

startGame player1 0 player2 chips2 = do
    tellAllPlayers (show player1 ++ " is out of chips")
    tellAllPlayers (show player2 ++ " wins with a total of: " ++ show chips2)
startGame player1 chips1 player2 0 = do
    tellAllPlayers (show player2 ++ " is out of chips")
    tellAllPlayers (show player1 ++ " wins with a total of: " ++ show chips1)
startGame player1 chips1 player2 chips2 = do
    tellAllPlayers "Dealing..."
    shuffled <- shuffleM deck
    let (cards1, rest) = splitAt 5 shuffled
        hand1 = pokerHand cards1
        cards2 = take 5 rest
        hand2 = pokerHand cards2
        p1 = PokerPlayer
            { player = player1
            , chips = chips1
            , cards = cards1
            , hand = hand1
            }
        -- Always start with a 1 chip ante from player 2
        pot = 1
        owed = 1
        p2 = PokerPlayer
            { player = player2
            , chips = chips2 - 1
            , cards = cards2
            , hand = hand2
            }
    tellPlayer player1 ("You have " ++ prettyHand cards1 ++ ", " ++ show hand1)
    tellPlayer player2 ("You have " ++ prettyHand cards2 ++ ", " ++ show hand2)
    wager p1 p2 pot owed

main = playMultiplayerGame "two player five card stud" 2 $ do
    tellAllPlayers "Welcome to two player five card stud!"
    [player1, player2] <- getPlayers
    startGame player1 20 player2 20

-- Let's talk about the betting phase. We'll be alternating between each
-- player. At each player's betting turn, he/she will be allowed to:
--
-- 1. Call, which would be to match whatever bet is on the table.
-- 2. Raise, which would match the current bet and add a little more.
-- 3. Fold
