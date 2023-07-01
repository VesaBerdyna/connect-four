module Board
  ( drawBoard
  , newBoard
  , place
  , isFinished
  , transpose
  , diagonals
  , isLineFull
  ) where

import Graphics.Gloss

-- |Create grid
grid :: [Picture]
grid = [translate (x*100 + 50) 0 $ color gridColor $ rectangleSolid 3 600 |
        x <- [-3..2]]

-- |Create the board
drawPlatter :: Picture
drawPlatter = pictures ([drawHole x y | x <- [0..6], y <- [0..5]] ++ grid)


-- |Place 1 at hole x y
drawPiece1 :: Float -> Float -> Picture
drawPiece1 x y = translate x' y' piece1
  where (x', y') = getHolePos x y

-- |Place 2 at hole x y
drawPiece2 :: Float -> Float -> Picture
drawPiece2 x y = translate x' y' piece2
  where (x', y') = getHolePos x y

-- |Create hole at position x y
drawHole :: Float -> Float -> Picture
drawHole x y = pictures [translate x' y' hole,
                         color empty $ translate (x' - 20) (y' + 37) $
                         scale 0.1 0.1 $ text txt]
  where
    (x', y') = getHolePos x y
    txt = "(" ++ show (round x) ++ ", " ++ show (round y) ++ ")"
    

-- |Draw piece for player i, act as a wrapper for drawPiece1/drawPiece2
drawPiece :: Int -> Int -> Int -> Picture
drawPiece i x y
  | i == 1 = drawPiece1 x' y'
  | i == 2 = drawPiece2 x' y'
  | otherwise = blank
  where
    x' = fromIntegral x
    y' = fromIntegral y

-- |Draw multiple pieces
drawPieces :: [[Int]] -> Picture
drawPieces b = pictures [drawPiece (b !! x !! y) x y |
                          x <- [0..6], y <- [0..5]]
                          
-- |End screen
drawEnd :: Picture
drawEnd = translate (-107) (-55) $ color white $ text "End"

-- |Draw the entire board
drawBoard :: [[Int]] -> Picture
drawBoard b
  | isFinished b = pictures [drawPlatter, drawPieces b, drawEnd]
  | otherwise    = pictures [drawPlatter, drawPieces b]

------ GAME ------

-- |Generate an empty new board, 0 = None, 1 = Player 1, 2 = Player 2
newBoard :: [[Int]]
newBoard = aux 7 -- 7 Columns
  where
    aux 0 = []
    aux n = [0,0,0,0,0,0] : aux (n-1) -- 6 rows


-- |A function that does this (in python): xs[n] = e
replace :: Int -> a -> [a] -> [a]
replace 0 e (_:xs) = e : xs
replace n e (x:xs) = x : replace (n-1) e xs
replace n e []     = []

-- |Place as player i at coordinates x y in board l
-- <=> l[x][y] = i
placeCoord :: Int -> Int -> Int -> [[Int]] -> [[Int]]
placeCoord i 0 y (l:ls) = replace y i l : ls
placeCoord i x y (l:ls) = l : placeCoord i (x-1) y ls
placeCoord i x y []     = []


-- |Add i at the bottum of a column (first "0" encoutered)
addInColumn :: Int -> [Int] -> [Int]
addInColumn i [] = [] -- <- Convention (should never happen in our game)
addInColumn i (x:xs)
  | x == 0 = i:xs
  | otherwise = x : addInColumn i xs

-- |Place as player i at column c.
place :: Int -> Int -> [[Int]] -> [[Int]]
place _ _ [] = [] -- <- Convention
place i 0 (x:xs)
  | isLineFull x = x:xs
  | otherwise = addInColumn i x : xs
place i c (x:xs) = x : place i (c-1) xs

-- |Check a line is full
isLineFull :: [Int] -> Bool
isLineFull [] = True
isLineFull (x:xs)
  | x == 0    = False
  | otherwise =  isLineFull xs

-- |Check if the board is full
isBoardFull :: [[Int]] -> Bool
isBoardFull = foldr ((&&) . isLineFull) True
