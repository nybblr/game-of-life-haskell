module Main where

import Gol

board = [[0,1,0],
         [0,1,0],
         [0,1,0]]

block = [[0,0,0,0],
         [0,1,1,0],
         [0,1,1,0],
         [0,0,0,0]]

beacon = [[0,0,0,0,0,0],
          [0,1,1,0,0,0],
          [0,1,1,0,0,0],
          [0,0,0,1,1,0],
          [0,0,0,1,1,0],
          [0,0,0,0,0,0]]

game = Gol.boardToCells board

nextGame = Gol.nextGeneration game

main :: IO ()
main = print nextGame
