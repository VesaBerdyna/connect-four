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



