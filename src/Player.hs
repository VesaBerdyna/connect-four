module Player (computerPlayer) where

import Board_Table
import Streaks


-- |Play with and depth d and board b
computerPlayer :: Int -> [[Int]] -> [[Int]]
computerPlayer depth board
  | count1s board == 1 = place 2 3 board -- Heuristic
  | otherwise      = pos
    where
      mt = genTree 2 depth board -- Move tree
      ct = genCoef 2 mt -- Coef tree
      ts = getSubNodes mt -- Moves allowed now
      tsc = getSubNodes ct -- Coef of moves allowed now
      mct = maxTree tsc -- Max coef
      pos = head [getVal (ts !! x) | x <- [0..length ts],
                  getVal (tsc !! x) == mct]


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


-- |Generate coeffs of POV player i
genCoef :: Int -> Tree [[Int]] -> Tree Int
genCoef i (Node b []) = Node (eval b) []
genCoef i (Node b ts) = Node (f x) x
  where
    n = if i == 1 then 2 else 1
    f = if i == 1 then minTree else maxTree
    x = [genCoef n t | t <- ts]

-- |Count all 1s in board
count1s :: [[Int]] -> Int
count1s = foldl (\acc x -> acc + aux x) 0
  where
    aux []     = 0
    aux (x:xs) = aux xs + if x == 1 then 1 else 0


