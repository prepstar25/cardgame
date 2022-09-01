module Blackjack where

import Types
import Data.List (intersperse)

won :: Money -> Money -> Money
Money x `won` Money y = Money $ x + y

lost :: Money -> Money -> Money
Money x `lost` Money y = Money $ x - y

rankToValue :: Rank -> Int
rankToValue r
    | r >= Ten = 10
    | otherwise = fromEnum r + 1

sumHand :: [Card] -> Int
sumHand cs = sum [value c | c <- cs]

hasAce :: [Card] -> Bool
hasAce (c:cx)
    | rank c == Ace = True
    | otherwise = hasAce cx
hasAce _ = False

bestValue :: [Card] -> Int
bestValue c
    | hasAce c = if (sumHand' + 10) <= 21 then sumHand' + 10 else sumHand'
    | otherwise = sumHand'
    where sumHand' = sumHand c

showHand :: String -> Bool -> [Card] -> String
showHand name showLast hand = do
    let handNice = intersperse ", " [(show $ rank c) ++ " of " ++ (show $ suit c) | c <- hand]
        showConditional = if showLast then handNice else init handNice ++ ["* of *"]
        showSum = if showLast then " (" ++ show (bestValue hand) ++ ")" else " (*)"
    name ++ ": " ++ (concat showConditional) ++ showSum

hit :: [Card] -> [Card] -> ([Card], [Card])
hit hand (c:deck) = (c:hand, deck)


-- Dealer hits until 17 or bust.
dealerHits :: [Card] -> [Card] -> ([Card], [Card])
dealerHits dealerHand deck
    | bestValue dealerHand < 17 = let (dh, d) = hit dealerHand deck in dealerHits dh d
    | otherwise = (dealerHand, deck)

-- 3 tuple extraction:

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

incRound :: Rounds -> Rounds
incRound (Rounds a) = Rounds (a + 1)