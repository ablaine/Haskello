module DataTypes where

import Data.Array

data State = X | O | E
	deriving (Eq, Enum, Show, Bounded)

type Board = Array Int (Array Int State)
type Point = (Int, Int)
type Move = (Point, State)

