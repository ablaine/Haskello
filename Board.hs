module Board
( board_bounds, board_size, othelloBoard
, makeMove
, withinBounds
, getState
, isValidMove
, points
) where

import Data.Ix
import Data.Array

import Utils
import DataTypes

{-- Constants --}
board_bounds = (0, 7)
board_size = rangeSize board_bounds

othelloBoard :: Board
othelloBoard = flipMult [(3,3),(4,4)] O (flipMult [(3,4),(4,3)] X blankBoard)

blankBoard = mkArray $ mkArray E
	where
		mkArray = listArray board_bounds . replicate board_size

{-- Manipulating the board --}
points :: State -> Board -> Int
points state = length . filter (==state) . concat . elems2D

makeMove :: Move -> Board -> Board
makeMove m@(_,s) b = flipMult (getFlipped m b) s b

getFlipped :: Move -> Board -> [Point]
getFlipped m@(p,s) b = if null result then [] else p:result
	where
		result = concatMap followInit $ listOfValidDir m b
			where
				followInit d = follow (addPair p d) []
					where
						follow p lst
							| withinBounds p && getState p b == oppState s = follow (addPair p d) (p:lst)
							| withinBounds p && getState p b == s = lst
							| otherwise = []

listOfDir = [ (a,b) | a <- [-1,0,1], b <- [-1,0,1], a /= 0 || b /= 0 ]

listOfValidDir :: Move -> Board -> [Point]
listOfValidDir (p,s) b = filter isValidDir listOfDir
	where
		isValidDir d = withinBounds p' && getState p' b == oppState s
			where
				p' = addPair p d

flipMult :: [Point] -> State -> Board -> Board
flipMult [] _ b = b
flipMult (x:xs) val b = flipMult xs val $ flipSingle x val b
	where
		flipSingle (x, y) val b = b // [(y,((b ! y) // [(x,val)]))]

withinBounds :: Point -> Bool
withinBounds (x, y) = check x && check y
	where
		check = inRange board_bounds

getState :: Point -> Board -> State
getState (x, y) b = b ! y ! x

isValidMove :: Board -> Move -> Bool
isValidMove board move = length (take 1 $ getFlipped move board) > 0

