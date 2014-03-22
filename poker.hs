{---------------------------------------------------------------------------------
author: RD Galang
For one of the "in-class exercises" I ended up writing a recursive solution
and that got me thinking, why don't I implement the same functions in Haskell?
So here it is.
---------------------------------------------------------------------------------}

{-
creates a standard 52-card deck using a list comprehension.
each "card" is represented by a tuple (a,b), where a = rank and b = suit
-}
deck :: [(Integer, Char)]
deck = [(a,b) | a <- [2..14], b <- ['H','D','S','C']]

{-
input: hand
returns a list of the ranks sorted in desc. order
Also handles the case where you have a 5-high straight, in which case ace is low
-}
card_ranks :: (Num a, Ord a) => [(a,t)] -> [a]
card_ranks [] = []
card_ranks xs
  | sortedList == [14,5,4,3,2] = [5,4,3,2,1]
  | otherwise = sortedList
  where
    ranks = extract_ranks xs
    sortedList = reverse $ quicksort ranks

{-
input: hand
determines if the hand is a flush
all it does is replicate the head of the string n times, where n is the
number of cards in the hand, and then check if that matches the whole string
-}
flush :: Eq t => [(a,t)] -> Bool
flush [] =  False
flush xs = let (y:ys) = extract_suits xs
               n = length xs
           in if replicate n y == (y:ys)
              then True
              else False

{-
input: hand
determines if the hand is a straight
it pulls the ranks and checks if the difference between min and max rank is 4
and that there are 5 cards in the hand. if both of these are true it's a
straight. obviously doesn't work with more than 5 cards
-}
straight :: (Num a, Ord a) => [(a, t)] -> Bool
straight [] = False
straight xs = let ranks = card_ranks xs
              in if (head ranks - last ranks == 4) && (length xs == 5)
                 then True
                 else False

{-
input: hand
a more flexible checker for straights in that it accepts more than 5 cards.
it requires that the hand have 5 or more cards.
currently doesn't support ace-low straights.
-}
flexible_straight :: [(Int, t)] -> Bool
flexible_straight [] = False
flexible_straight xs = let ranks = card_ranks xs
                           n = length xs
                       in if (n>=5) && ((head ranks - last ranks) == (n - 1))
                          then True
                          else False

{-
input: ranks
given a number n and a list of ranks, if there is an n-of-a-kind in the ranks then
the rank is returned.
ex: 2 [5,1,3,6,3] -> Just 3
-}
kind :: (Eq a, Eq a1, Num a1) => a1 -> [a] -> Maybe a
kind n [] = Nothing
kind n (x:xs) = let occurences = sum [1 | match <- (x:xs), match == x]
                in if occurences == n
                   then Just x
                   else kind n xs
{-
input: ranks
if a two pair exists, then it returns a tuple of the pair. it assumes that the
input has already been sorted by card_ranks.
ex: [8,8,6,4,4] -> Just (Just 8, Just 4)
-}
two_pair :: Eq t => [t] -> Maybe (Maybe t, Maybe t)
two_pair [] = Nothing
two_pair xs = let pair = kind 2 xs
                  lowpair = kind 2 $ reverse xs
              in if (pair /= Nothing) && (lowpair /= Nothing) && (pair /= lowpair)
                 then Just (pair, lowpair)
                 else Nothing

{-
utility function to pull only the ranks from a list of card tuples
ex: [(5,'H'), (9,'D'), (7,'C'),(2,'H'),(3,'S')] -> [5,9,7,2,3]
-}
extract_ranks :: [(a,t)] -> [a]
extract_ranks [] = []
extract_ranks ((a,b):xs) = a : extract_ranks xs

{-
similar utility function as extract_ranks but with suits
ex: [(5,'H'), (9,'D'), (7,'C'),(2,'H'),(3,'S')] -> "HDCHS"
-}
extract_suits :: [(a,t)] -> [t]
extract_suits [] = []
extract_suits ((a,b):xs) = b : extract_suits xs


--from Learn You a Haskell
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted
