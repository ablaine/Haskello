module Main where

import System.IO
import System.Cmd

import DataTypes
import Board
import Utils

main :: IO ()
main = defaultGame 

defaultGame = play othelloBoard


play :: Board -> IO a
play board = do
	system "clear"
	pp board
	move <- getValidInput X board "Dark"
	system "clear"
	pp $ makeMove (move,X) board
	otherMove <- getValidInput O board "Light"
	play $ makeMove (otherMove, O) $ makeMove (move, X) board

--TODO: Cleanup/improve; Parse for exit/quit and stuff
getValidInput :: State -> Board -> String -> IO Point
getValidInput s b string = do
	move <- getInput string
	if isValidMove b (move,s)
		then return move
		else do
			putStrLn "Sorry, please choose a valid move."
			getValidInput s b string
	where
		getInput :: String -> IO Point
		getInput p = do
			putStr $ p ++ "'s move [ x y ]: "
			hFlush stdout
			move <- getLine
			if withinBounds $ parseInput move
				then return $ parseInput move
				else do
					putStrLn "Sorry, please choose two indicies in the range [0,7]"
					getInput p
			where
				parseInput :: String -> Point
				parseInput str = (lst !! 0, lst !! 1)
					where
						lst = map read $ words str::[Int]

