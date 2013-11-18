module Helper.WinningTests where

import Helper
import Helper.Winning
import Helper.Pretty
import Test.Hspec
import qualified Data.Map as Map
import Control.Arrow ((&&&))

main :: IO ()
main = hspec $ do
    let go x' y =
            it (prettyHand x) $ pokerHand x `shouldBe` y
          where
            x = map parseCard $ words x'
    go "AC 2C 3C 4C 5C" (StraightFlush Five)
    go "AD 2C 3C 4C 5C" (Straight Five)
    go "6C 2C 3C 4C 5C" (StraightFlush Six)
    go "7C 2C 3C 4C 5C" (Flush [Seven, Five, Four, Three, Two])
    go "AC AD 3C 8D KS" (Pair Ace [King, Eight, Three])
    go "AC AD 3C 3D 8D" (TwoPair Ace Three Eight)

cardMap = Map.fromList $ map (prettyCard &&& id) deck

parseCard s =
    case Map.lookup s cardMap of
        Just c -> c
        Nothing -> error $ "Invalid card: " ++ s
