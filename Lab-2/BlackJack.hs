module BlackJack where
import Cards
import Wrapper
import System.Random
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

-- Helper function that calculates the value of given hand
-- using the score of the hands' cards (Aces always 11)
valueHand :: Hand -> Integer
valueHand Empty            = 0
valueHand (Add card hand)  = valueCard card + valueHand hand

-- Calculates the value of a Hand
-- If the value of a Hand exceeds 21 and a hand has Aces
-- it uses value of 1 for every Ace
value :: Hand -> Integer
value hand | score > 21 = score - (10 * aces)
           | otherwise = score
  where score = valueHand hand
        aces  = numberOfAces hand

-- Given a hand, is the player bust?
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- Determines the winner among a guest and a bank
winner :: Hand -> Hand -> Player
winner guest bank | not (gameOver guest) && value guest > value bank = Guest
                  | not (gameOver guest) && gameOver bank            = Guest
                  | otherwise                                        = Bank

-- Merges two hands by putting the first hand on top of the second one
(<+) :: Hand -> Hand -> Hand
Empty         <+ h2 = h2
(Add card h1) <+ h2 = Add card (h1 <+ h2)

-- Checks associativeness of the <+ function
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- The size of the combined hand should be
-- the sum of the sizes of the two individual hands
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size (p1 <+ p2) == size p1 + size p2

-- Returns a full deck of cards listing 52 cards in total
fullDeck :: Hand
fullDeck =
  foldr (<+) Empty crossover
  where crossover = map fullSuits [Hearts, Spades, Diamonds, Clubs]

-- Returns a partial deck containing all cards of a suit
fullSuits :: Suit -> Hand
fullSuits s =
  foldr (<+) Empty cardList
  where cardList =  [Add (Card(Numeric i) s) Empty | i <- [2..10]]
                    ++ [Add (Card p s) Empty | p <- [Ace, King, Jack, Queen]]

-- Draws a card from a deck
-- and puts it into the hand while erroring on an empty deck
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty h = error "draw: The deck is empty."
draw (Add c d) h = (d, Add c h)

-- Plays a bank starting with an empty hand.
-- Draws cards until its score is 16 or higher.
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

-- Helper function playing a hand with a deck until its score is 16 or higher
playBank' :: Hand -> Hand -> Hand
playBank' deck hand | value hand >= 16 = hand
                    | otherwise       = uncurry playBank' play
  where play = draw deck hand

-- Shuffles a hand of cards using a random number generator
shuffle :: StdGen -> Hand -> Hand
shuffle _ Empty      = Empty
shuffle g unShuffled = Add removed (shuffle g' partial)
   where
      handSize               = size unShuffled
      (idx, g')              = randomR (1, handSize) g
      (partial, removed)     = removeCard unShuffled idx Empty

-- Removes a card from a hand1 at given position and moves it to the hand2.
-- Returns a tuple of the changed Hand1 and removed card
removeCard :: Hand -> Integer -> Hand -> (Hand, Card)
removeCard (Add card h1) idx h2
   | idx == 1    = (h1 <+ h2, card)
   | otherwise   = removeCard h1 (idx-1) (Add card h2)

-- Checks if a Card belongs to a Hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty      = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Checks that a card remains in a hand after shuffling a hand
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
   c `belongsTo` h == c `belongsTo` shuffle g h

-- Checks that the size of the hand is preserved by shuffle
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand =
   size hand == size (shuffle g hand)

implementation = Interface {
   iEmpty = empty
   , iFullDeck = fullDeck
   , iValue = value
   , iGameOver = gameOver
   , iWinner = winner
   , iDraw = draw
   , iPlayBank = playBank
   , iShuffle = shuffle
}

main :: IO ()
main = runGame implementation