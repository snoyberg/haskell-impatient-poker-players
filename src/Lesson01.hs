module Lesson01 where

-- In order to get started, we need to define some data types.
-- A lot of the programming you'll do in Haskell starts by
-- clearly defining your data, and then writing functions to
-- manipulate that data. We'll start with a type to represent
-- a card's suit.
data Suit = Club | Diamond | Heart | Spade
-- What this means is that the Suit type can be constructed in
-- one of four ways. Club, Diamond, Heart, and Spade are therefore
-- known as data constructors. The vertical line between them
-- (aka, the pipe, typed by pressing shift-backslash) means "or".
-- In other words, a suit is a club, *or* a diamond, *or* a heart,
-- *or* a spade.

-- We want to be able to show our suit and check if it equals another
-- value. We use a deriving clause to achieve that, which for the moment
-- we can treat as magic (we'll define "magic" better in a later tutorial).
    deriving (Show, Eq, Ord, Enum, Bounded)

-- I derived five different typeclasses here. Let me give a quick explanation
-- of each one:

-- Show: this type can be converted to some text that can be shown to the user.
-- Eq: We can test if two values are equal. This allows us to write Club == Club, for example.
-- Ord: Ability to order our suits. This makes more sense when we talk about ranks below.
-- Enum: We'd like to be able to *enumerate* all of the different suits. Enum allows
--       for that. We'll use this functionality below.
-- Bounded: There's a minimum and maximum value for this datatype. In this case, Club and Spade.

-- Next we want to create a datatype for rank, which can be the numbers
-- two through ten, plus the four face cards. We could use a normal
-- integral value to represent this, but creating another datatype like
-- we have for suite works out better in practice.
data Rank = Two | Three | Four | Five | Six | Seven | Eight
          | Nine | Ten | Jack | Queen | King | Ace
   deriving (Show, Eq, Ord, Enum, Bounded)

-- Both Suit and Rank are known as enumeration types; a single data type with
-- a lot of different data constructors, where each constructor carries no extra
-- data. But that's not the only thing we can do with datatypes. Another common
-- data type is one where there's just a single data constructor, but that data
-- constructor has multiple fields to hold more data. And that's exactly what
-- we're going to define a card as:
data Card = Card Suit Rank
    deriving (Show, Eq, Ord)

-- What we're saying is that a card is composed of both a suit and a rank. It's
-- common in these cases to use the same name for the data type and the data
-- constructor.

-- OK, we've got three data types, let's do something useful with them. I'd like
-- to create a deck of cards. That turns out to be really easy:
deck = do
    suit <- [minBound..maxBound]
    rank <- [minBound..maxBound]
    return (Card suit rank)

-- That may have looked like a bit of magic, so let's explain it. [minBound..maxBound]
-- uses those Enum and Bounded typeclasses I mentioned early to create a list of
-- every possible value for a specific data type. Then `suit <-` says "give them to me,
-- one at a time." We do the same to get each rank, and then for each suit/rank combination,
-- create a new card and return it.

-- Let's finish this all off by creating a runnable program here. By convention, our program
-- always starts with a function called main. Let's write a program that will print out a deck:
main = print deck

-- In order to run this, click on the target selector, which is to the right of the play triangle
-- on the top bar. Then select Lesson01, and click on the play triangle. You should see 52
-- cards printed to the console tab at the bottom.

-- Congratulations! You've completed the first lesson. You can continue with some exercises below,
-- or jump right into lesson 2 by selecting that module to the left. (Don't forget to change the
-- target module when you start working on a new lesson.)

{-

Exercises:

1. Instead of using minBound and maxBound for the suit, use the Club and Spade. The
   result should be the same.

2. There's another game I like to play called Euchre, which is played with only the cards
   9 and up. Modify the line `rank <- [minBound..maxBound]` so that it only creates
   the ranks 9 through ace.

3. There's a different syntax for lists which does not use the two dots. For example, to
   create the list of odd numbers from 1 through 7, I could write: [1, 3, 5, 7].
   Modify the suit line above to use this syntax, and only include the red suits.

-}