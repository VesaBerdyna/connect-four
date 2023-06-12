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
