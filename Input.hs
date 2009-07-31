module Input
( getNextMove
) where

import Data.Char

import DataTypes
import Board

getNextMove :: State -> Board -> IO Point
getNextMove s b = do
	move <- parseInput
	if isValidMove (move, s) b
		then return move
		else do
			putStrLn "Sorry, please choose a valid move."
			getNextMove s b

parseInput :: IO Point
parseInput = do
	inStr <- getLine
	case length (words inStr) of
		1 ->
			case map toLower inStr of
				"moves" -> do
					putStrLn "TODO: Not implemented yet."
					parseInput
				otherwise -> do
					putStrLn "Try again"
					parseInput
		2 -> do
			let points = map fst $ concatMap (reads :: ReadS Int) $ words inStr
			if length points == 2
				then do
					let move = (points !! 0, points !! 1)
					if withinBounds move
						then return move
						else do
							putStrLn $ "Sorry, your move " ++ show move ++ " is out of bounds. [0,7]"
							parseInput
				else do
					putStrLn "Need two NUMBERS!"
					parseInput
		otherwise -> do
			putStrLn "Try again"
			parseInput


