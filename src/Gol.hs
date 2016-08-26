module Gol where

import Data.List (genericLength, nubBy, sortOn)

-- Live, <2 = DEAD
-- Live, 2 OR 3 = LIVE
-- Live, >3 = DEAD
-- Dead, =3 = LIVE

nextGeneration :: [Cell] -> [Cell]
nextGeneration board = (filter isLive ((\ cs -> map (nextCell cs) cs) (uniqueByPositionPreferringLive (concatMap cellWithNeighbors board))))

cellWithNeighbors :: Cell -> [Cell]
cellWithNeighbors c = c : map (deadCellByAddingToPosition c) neighborDeltas
  where
    neighborDeltas = [ (x, y) | x <- [-1..1], y <- [-1..1]]

    deadCellByAddingToPosition (Cell _ (px, py)) (dx, dy) = Cell False (px + dx, py + dy)

uniqueByPositionPreferringLive :: [Cell] -> [Cell]
uniqueByPositionPreferringLive cs = nubBy isEqual (sortOn (not . isLive) cs)
  where
    isEqual (Cell _ p1) (Cell _ p2) = p1 == p2

boardToCells :: [[Integer]] -> [Cell]
boardToCells board = [ Cell True (x,y) | x <- [0..(w-1)], y <- [0..(h-1)], board !! y !! x == 1 ] where
  h = genericLength board
  w = genericLength (head board)

data Cell = Cell Bool (Int, Int)
  deriving (Show)

isLive :: Cell -> Bool
isLive (Cell b _) = b

countLive :: [Cell] -> Integer
countLive cs = genericLength (filter isLive cs)

nextCell :: [Cell] -> Cell -> Cell
nextCell cs c@(Cell _ p) = Cell (decideLive (isLive c) (countLive (neighbors c cs))) p

neighbors :: Cell -> [Cell] -> [Cell]
neighbors (Cell _ p@(x, y)) cs = filter isNeighbor cs
  where
    isNeighbor (Cell _ p'@(x', y')) = abs(x - x') <= 1 && abs(y - y') <= 1 && p /= p'

decideLive :: Bool -> Integer -> Bool
decideLive False 3 = True
decideLive True n
  | n == 2 || n == 3 = True
decideLive _ _ = False
