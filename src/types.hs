module Types where

data Suit = Spades | Hearts | Clubs | Diamonds deriving (Show, Enum)
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Eq, Ord, Enum)
data Card = Card {rank :: Rank
                    , value :: Int
                    , suit :: Suit
                    } deriving (Show)

newtype Money = Money Integer deriving (Eq, Ord)
instance Show Money where show (Money x) = show x

newtype Rounds = Rounds Integer deriving (Eq, Ord)
instance Show Rounds where show (Rounds x) = show x


--to implement state
data Game     = Game
  { _gPlayerHand    :: [Card]
  , _gComputerHand  :: [Card]
  , _gPlayerBank    :: Money
  , _gRounds        :: Rounds
  } deriving Show  