module Main where

import System.Cmd

import DataTypes
import Board
import Utils
import Input

main :: IO ()
main = play defaultStart 

defaultStart = GameState othelloBoard X 2 2 4

play env = do
	let b = board env
	let s = curTurn env
	let s' = oppState s
	if hasValidMove s b
		then do
			refreshScreen
			requestMove s
		else if hasValidMove s' b
			then do
				refreshScreen
				putStrLn $ stateToStr s ++ " was unable to make any move."
				requestMove s'
			else do
				refreshScreen
				putStrLn "Game is over!"
	where
		refreshScreen = do
			system "clear"
			putStrLn "<<   Haskello   >>"
			printScore (darkPts env) (lightPts env)
			printBoard (board env)
			putStr "\n"
		requestMove player = do
			let b = board env
			let s = player
			let s' = oppState s
			printRequestToMakeMove s
			move <- getNextMove s b
			let b' = makeMove (move, s) b
			play $ GameState b' s' (points X b') (points O b') . succ $ totalPts env

