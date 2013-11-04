{-
   Lab Assignment 2
   Anna Averianova & Tobias Deekens, 2013
-}

{-
   size hand2
     = size (Add(Card(Numeric 2) Hearts))
                (Add (Card Jack Spades) Empty)
     = ...
     = 2
-}

module BlackJack where
import Cards
import Wrapper

-- Returns an empty hand
empty :: Hand
empty = Empty

-- Calculates the value of a Rank
-- for Ace:
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Jack        = 10
valueRank Queen       = 10
valueRank King        = 10
valueRank Ace         = 11

-- Calculates the value of a Card
valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r

-- Calculates the number of aces in a given hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add card hand) = numberOfAces hand

-- Calculates the value of the hand
value :: Hand -> Integer
value Empty = 0
value (Add card hand) = valueCard card + value hand

hand5 = Add (Card(Numeric 7) Diamonds) (Add (Card Jack Hearts) (Add (Card Jack Spades) Empty))

-- Given a hand, is the player bust?
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- Defines a winner among the guest and the bank
winner :: Hand → Hand → Player 