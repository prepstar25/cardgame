module Impure where

import Blackjack
import Types
import SimpleMath


import Data.Char
import Text.Read (readMaybe)

doubleDown :: Money -> IO Money
doubleDown balance = do
    readValidBet balance

readHitStay :: (Rounds,Money,Money) -> [Card] -> [Card] -> IO ([Card], [Card], Money)
readHitStay (gamerounds,cbet,cmony) playerHand deck = do
    putStrLn $ "Round  :" ++ show (fst3(gamerounds,cbet,cmony))
    putStrLn $ "Bet    :" ++ show (snd3(gamerounds,cbet,cmony))
    putStrLn $ "Balance:" ++ show (trd3(gamerounds,cbet,cmony))

    if length playerHand < 3 && subM(cmony, cbet) > Money 0 then
        putStrLn "(h)it or (s)tay or (d)oubledown: "
    else
        putStrLn "(h)it or (s)tay: "

    hitOrStay <- getLine
    let uprHitorStay = map toUpper hitOrStay
    case uprHitorStay of
        ""  -> readHitStay (gamerounds,cbet,cmony) playerHand deck
        "H" -> pure (fst(hit playerHand deck),snd(hit playerHand deck), cbet)
        "S" -> pure (playerHand, deck, cbet)
        "D" -> do
                if length playerHand > 2 || subM(cmony, cbet) < Money 1 then
                    readHitStay (gamerounds,cbet,cmony) playerHand deck
                else  do  
                    dDown <- doubleDown $ subM(cmony, cbet)
                    putStrLn $ "New Bet    :" ++ show (addM(cbet,dDown))
                    pure (playerHand,deck,addM(cbet,dDown))
        _   -> readHitStay (gamerounds,cbet,cmony) playerHand deck

--Hit until stay or bust
playerHitStay :: (Rounds,Money,Money) -> [Card] -> [Card] -> IO ([Card], [Card], Int, Money)
playerHitStay (gamerounds,cbet,cmony) playerHand deck = do
    putStrLn $ showHand "Player" True playerHand

    if  playerHandValue >= 21 then --stop if 21 or bust
        pure (playerHand, deck, playerHandValue, cbet)
    else do
        (ph, d, b) <- readHitStay (gamerounds,cbet,cmony) playerHand deck
        if length ph == length playerHand then -- Player pressed 's'
            pure (playerHand, deck, playerHandValue, b)
        else
            playerHitStay (gamerounds,b,cmony) ph d
    where playerHandValue = bestValue playerHand

validateBet :: Money -> Money -> IO Money
validateBet bet playerMoney
    | bet > Money 0 && bet <= playerMoney = return bet
    | otherwise = do
        putStrLn $ "Your bet wasn't between 1 and " ++ show playerMoney ++ "!"
        readValidBet playerMoney

readValidBet :: Money -> IO Money
readValidBet playerMoney = do
    putStrLn "Please enter your bet: "
    bet <- fmap Money . readMaybe <$> getLine
    case bet of
        Nothing -> readValidBet playerMoney
        Just n -> validateBet n playerMoney

validateBank :: Money -> IO Money
validateBank bank
    | bank > Money 0 = return bank
    | otherwise = do
        putStrLn $ "Amount entered must be greater than 0"
        readValidStartBank

readValidStartBank :: IO Money
readValidStartBank = do
    putStrLn "Please enter your starting balance: "
    startBank <- fmap Money . readMaybe <$> getLine
    case startBank of
        Nothing -> readValidStartBank
        Just n -> validateBank n

initStartBalance :: IO Money
initStartBalance = readValidStartBank