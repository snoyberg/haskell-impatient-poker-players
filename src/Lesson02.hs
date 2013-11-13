module Lesson02 where

-- Each of our lessons is going to end up reusing a lot of the
-- same code, like the definitions of Suit and Rank. Instead of
-- copy-pasting all of that, I've created a module called Helper
-- that contains that shared code. Then we just need to *import*
-- that module to make those data types, functions, and values
-- available here.
import Helper

-- In this lesson, we're going to want to shuffle our deck and
-- deal out a few hands for 5-card stud. To do so, let's import
-- a helper library that will let us shuffle:
import System.Random.Shuffle

-- If you want more information about that module, click on it and
-- then click the "Info of identifier" icon below (the "i" just above
-- the messages tab).

main = do
    -- We defined deck in Helper, but it's always in the same order.
    -- Let's get a randomly shuffled deck.
    shuffled <- shuffleM deck

    -- shuffled is now a list of Cards. If you want to see that,
    -- double-click on the word shuffled, and in the Messages tab
    -- you should see:
    --
    --     shuffled :: [Card]
    --
    -- The :: means "is of type", and the square brackets around "Card"
    -- means "a list of." Altogether, that means "shuffled is of type
    -- list of Card," which is just a stilted way of saying "shuffled is
    -- a list of cards."
    --
    -- Now that we have a list of shuffled cards, we want to take the first
    -- five cards and "deal" it to player one, then the next five cards and
    -- deal to player two. To do that, we'll use splitAt:
    let (player1, rest) = splitAt 5 shuffled

    -- player1 is now a list with 5 cards, and rest is a list with the other 47.
    -- One important thing to mention is that the original shuffled list *didn't
    -- change.* Haskell has something called immutable data. Instead of destroying
    -- the old list of cards, we've created two new lists of cards based on the
    -- original.
    --
    -- Now let's get the second player's hand. We *could* use splitAt, but we don't
    -- actually care about the rest of the deck in this case. Instead, we'll use the
    -- simply take function:
    let player2 = take 5 rest

    -- Notice that we took the 5 cards from rest, not shuffled. Think about what
    -- would happen if we took from shuffled instead. Then try changing the code
    -- and see if your assumptions were correct.

    -- Alright, time to print out the hands:
    putStrLn "Player 1:"
    print player1
    putStrLn "Player 2:"
    print player2

{-

Remember: make sure to switch your target module before trying to run this code.

Exercises:

1. Change player2 to be take 5 shuffled, and see what the result is.
2. Try and modify the example to have 3 players. (A generalized version of
   this exercise will be our goal in lesson 3.)
3. Replace putStrLn with print and see how that changes the output.

Behind the scenes:

Let's answer some easy questions first. putStrLn means "put a string and a line." In
most programming languages, a String is a piece of text, or a list of characters.
In order to make the next bit of text appear on its own line, we need to output a
newline character at the end. putStrLn does that for you automatically. (Yes, there's
also a putStr function that doesn't do that.)

The print function does something else. It first "show"s your data. In other words, it
converts your data to a String form. If you look back in lesson 1, one of our
deriving clauses was for Show. That was Haskell's way of saying "just figure out how to
convert this to a string automatically, I don't care."

Now let's ask a few other questions:

* Why is the function called shuffleM, and not shuffle?
* Why do we sometimes use "let," and sometimes use that funny "<-" symbol?
* What's up with that word "do?"

In Haskell, we have a distinction between "pure" and "impure" functions. The latter
is often thought of as actions, or things which have some kind of side effect on the rest
of the world. I won't get into the theory of why this is a good distinction to make for
now.

So let's describe some things that would be consider side effects. One would be "we displayed
some data to the user." Perhaps surprisingly, another would be "we got some random data." The
reason this is impure is that, if we call that function a second time, it will give a different
result. Since shuffling needs to be random, each call needs to have a different result, and
therefore shuffling is in fact an impure action. Therefore, the function is called shuffleM:
the M at the end stands for "monad," which is our scary-Haskell-way of saying "has some context,
such as allowing side effects."

This also explains our other two questions. The "<-" symbol means "please run some impure function,
and get the result out of it." let, on the other hand, means "here's some pure function, get the
result." You might be wondering if we really need two different ways of doing this. The answer
is that this setup really is very useful, but it'll take a bit more time before I can demonstrate
why, so please just take my word on it for now.

The word "do" is also part of this whole setup. It says, "I'm about to start running a bunch of
things, one per line. Some of them will be actions, some will be actions that produce values that
I care about (and I'll use <- to capture those values), and sometimes I'll just use let to capture
pure results."

One of the most confusing things for beginning Haskellers is knowing when to use <-, when to use let,
and what to start do notation. Since this is a tutorial for the impatient, I'm not going to explain
those rules right now, just give you the general guidelines I've already mentioned, and teach by example.
Don't worry, by the time you finish this tutorial, the distinction should be second nature for you.

-}