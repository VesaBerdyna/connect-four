module Computer (playComputer) where

import Board

-- |Count streaks of n consecutive piece of player i, k is the counter,
-- p previous elem
lineStreak :: Int -> Int -> Int -> Int -> [Int] -> Int
lineStreak n 0 i _ b = 1 + lineStreak n n i 0 b
lineStreak _ _ _ _ [] = 0
lineStreak n k i z (x:xs)
  | x == i && x == z = lineStreak n (k-1) i x xs
  | x == i && k == n = lineStreak n (k-1) i x xs
  | otherwise        = lineStreak n n i x xs

verticalStreak :: Int -> Int -> [[Int]] -> Int
verticalStreak n i = foldl (\acc x -> acc + lineStreak n n i 0 x) 0

horizontalStreak :: Int -> Int -> [[Int]] -> Int
horizontalStreak n i = verticalStreak n i . transpose

diag1Streak :: Int -> Int -> [[Int]] -> Int
diag1Streak n i = verticalStreak n i . diagonals

diag2Streak :: Int -> Int -> [[Int]] -> Int
diag2Streak n i = verticalStreak n i . diagonals . reverse

streaks :: Int -> Int -> [[Int]] -> Int
streaks n i b = verticalStreak n i b +
                horizontalStreak n i b +
                diag1Streak n i b +
                diag2Streak n i b

-- |Evaluation function for player 2 (bot)
eval :: [[Int]] -> Int
eval b = 100 * streaks 4 2 b + 5 * streaks 3 2 b + 2 * streaks 2 2 b
       - 1000 * streaks 4 1 b - 5 * streaks 3 1 b - 2 * streaks 2 1 b

-- |Tree data type
data Tree a = Node a [Tree a]
  deriving Show


-- |Generate tree of player i moves out of board b with depth d
genTree :: Int -> Int -> [[Int]] -> Tree [[Int]]
genTree i 0 b = Node b []
genTree i d b = Node b [aux x | x <- [0..6], not $ isLineFull (b !! x)]
  where
    aux a = genTree (if i == 1 then 2 else 1) (d-1) $ place i a b

-- |Get Value of a Node
getVal :: Tree a -> a
getVal (Node a _) = a

-- |Get nodes below
getSubNodes :: Tree a -> [Tree a]
getSubNodes (Node _ a) = a

-- |Find maximum of tree of ints
maxTree :: [Tree Int] -> Int
maxTree = foldl (\acc x -> max acc $ getVal x) (-1000)

-- |Find minimum of tree of ints
minTree :: [Tree Int] -> Int
minTree = foldl (\acc x -> min acc $ getVal x) 1000
