module DataTypes where

import Data.Array

data State = X | O | E
	deriving (Eq, Enum, Show)

type Board = Array Int (Array Int State)
type Point = (Int, Int)
type Move = (Point, State)

data GameState = GameState
	{ board :: Board
	, curTurn :: State
	, darkPts :: Int
	, lightPts :: Int
	, totalPts :: Int
	} deriving (Show)
