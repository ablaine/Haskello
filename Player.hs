module Player where

import System.Random

import DataTypes
import Board
import Input

humanMakeMove :: State -> Board -> IO Point
humanMakeMove = getNextMove

randomMakeMove :: State -> Board -> IO Point
randomMakeMove myState board = do
	let validMoves = getValidMoves myState board
	randNum <- randomIO
	return $ validMoves !! (randNum `mod` length validMoves)
