module BlackJack where
import Cards
import Wrapper
import Test.QuickCheck

{-
   Lab Assignment 2
   Anna Averianova & Tobias Deekens, 2013
-}

{-
   # Task 3.2

   size hand2
     = size (Add(Card(Numeric 2) Hearts))
                (Add (Card Jack Spades) Empty) =
     = 1 + size (Add (Card Jack Spades) Empty) =
     = 1 + 1 + size Empty =
     = 1 + 1 + 0 =
     = 2
-}

-- # Task 3.3

-- Returns an empty hand
empty :: Hand
empty = Empty

-- Calculates the value of a Rank
-- Uses the default value of 11 for an Ace
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _           = 10

-- Calculates the value of a Card
valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r

-- Calculates the number of Aces in a given Hand
numberOfAces :: Hand -> Integer
numberOfAces Empty                   = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand)            = numberOfAces hand

-- Calculates the value of a Hand
-- If the value of a Hand exceeds 21 and a hand has Aces
-- it uses value of 1 for every Ace
value (Add card hand) | handValue > 21 = handValue - acesInHand * 10
                      | otherwise      = handValue
   where handValue   = valueCard card + value hand
         acesInHand  = numberOfAces (Add card hand)

-- Given a hand, is the player bust?
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- Determines the winner among a guest and a bank
winner :: Hand -> Hand -> Player
winner guest bank | not (gameOver guest) && value guest > value bank = Guest
                  | gameOver bank                                    = Guest
                  | otherwise                                        = Bank

-- Merges two hands by putting the first hand on top of the second one
(<+) :: Hand -> Hand -> Hand
Empty         <+ h2 = h2
(Add card h1) <+ h2 = (Add card (h1 <+ h2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size (p1 <+ p2) == size p1 + size p2

-- Returns a full deck of cards listing 52 cards in total
--fullDeck :: Hand
-- foreach 2..10 (is a Card) fullSuits
-- foreach Rank fullSuits

-- Returns a partial deck containing all suits for a card
--fullSuits :: Card
-- foreach Suit-type pair with Card

h1 = (Add (Card Ace Spades) (Add (Card(Numeric 5) Diamonds) (Add (Card Jack Diamonds) (Add (Card Ace Spades) Empty))))
h2 = (Add (Card(Numeric 5) Diamonds) (Add (Card Jack Diamonds) (Add (Card Ace Spades) Empty)))
h3 = (Add (Card(Numeric 10) Diamonds) (Add (Card Jack Diamonds) (Add (Card Jack Spades) Empty)))
h4 = (Add (Card Ace Spades) (Add (Card Jack Diamonds) (Add (Card Jack Spades) Empty)))

h5 = (Add (Card(Numeric 5) Diamonds) Empty)
h6 = (Add (Card Jack Diamonds) (Add (Card Ace Spades) Empty))
h7 = (Add (Card(Numeric 7) Spades) Empty)