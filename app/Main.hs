module Main (main) where

import Blackjack
import Types
import Impure
import System.Random.Shuffle (shuffleM)
import System.Console.ANSI

bettingRound :: Rounds -> Money -> IO Money
bettingRound gamerounds playerMoney = do
    putStrLn "\n"

    if playerMoney == Money 0 then do
        putStrLn "You're broke get out of the casino!"
        pure (Money 0)
    else do
        putStrLn $ "---- Round " ++ show gamerounds
        putStrLn $ "You have $" ++ show playerMoney

        -- Possible Cards
        let cards = [Card r (rankToValue r) s | s <- [Spades ..], r <- [Ace ..]]

        -- Shuffle the deck!
        let shuffledCards = shuffleM cards :: IO [Card]

        -- Deal the hands
        shuffled <- shuffledCards
        let userHandAndRest = splitAt 2 shuffled
        let computerHandAndRest = splitAt 2 (snd userHandAndRest)
        let deck = snd computerHandAndRest

        let computerHand = fst computerHandAndRest
        let playerHand = fst userHandAndRest

        -- Player bets
        betMade <- readValidBet playerMoney
        putStrLn $ "You bet: $" ++ show betMade

        -- show Computer hands, hide 2nd card
        putStrLn $ showHand "Computer" False computerHand

        -- Player can hit or stay or double down until their turn ends
        (finalPlayerHand, finalDeck, playerValue, dBet) <- playerHitStay (gamerounds,betMade,playerMoney) playerHand deck

        -- Show player hand
        if playerValue == 21 then do
            putStrLn "Blackjack! Player wins!"
            bettingRound (incRound gamerounds) $ playerMoney `won` dBet
        else if playerValue > 21 then do
            putStrLn "Bust! Player loses!"
            bettingRound (incRound gamerounds) $ playerMoney `lost` dBet
        else do
            putStrLn $ showHand "Player" True finalPlayerHand

            -- Dealer turn to hit until 17 or bust
            let finalDealerHand = fst $ dealerHits computerHand finalDeck
            putStrLn $ showHand "Computer" True finalDealerHand

            if bestValue finalDealerHand > 21 then do
                putStrLn "Dealer busted! Player wins!"
                bettingRound (incRound gamerounds) $ playerMoney `won` dBet 
            else if bestValue finalDealerHand == 21 then do
                putStrLn "Dealer blackjack! Dealer wins!"
                bettingRound (incRound gamerounds) $ playerMoney `lost` dBet
            else if bestValue finalDealerHand > playerValue then do
                putStrLn "Dealer better hand! Dealer wins!"
                bettingRound (incRound gamerounds) $ playerMoney `lost` dBet
            else do 
                putStrLn "Player better hand! Player wins!"
                bettingRound (incRound gamerounds) $  playerMoney `won` dBet 


main :: IO ()
main = do
    clearScreen
    let rounds = Rounds 0
    playerBank <- initStartBalance
    bettingRound rounds playerBank
    putStrLn "Game over!"