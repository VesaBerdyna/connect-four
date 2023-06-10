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
