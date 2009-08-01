module Main where

import System.Cmd

import DataTypes
import Board
import Utils
import Input

main :: IO ()
main = play defaultStart 

defaultStart = GameState othelloBoard X 2 2 4

header = "<<   Haskello   >>"

play :: GameState -> IO a
play env = do
	let b = board env
	let s = curTurn env
	let tPts = "T: " ++ show (totalPts env)
	system "clear"
	putStrLn header
	printScore (darkPts env) (lightPts env)
	printBoard b
	printRequestToMakeMove s
	move <- getNextMove s b
	let board' = makeMove (move, s) b
	let nextTurn = oppState s
	play $ GameState board' nextTurn (points X board') (points O board') . succ $ totalPts env


