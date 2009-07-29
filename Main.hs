module Main where

import System.IO
import System.Cmd

import DataTypes
import Board
import Utils
import Input

main :: IO ()
main = play defaultStart 

defaultStart = GameState othelloBoard X 2 2 4
header = "<<   Haskello   >>\n"

play :: GameState -> IO a
play env = do
	let b = board env
	let s = curTurn env
	let dPts = "X: " ++ show (darkPts env)
	let lPts = "O: " ++ show (lightPts env)
	let tPts = "T: " ++ show (totalPts env)
	let bPrint = showBoard b ++ ["",dPts ++ "\t" ++ lPts ++ "\t" ++ tPts]
	system "clear"
	putStrLn header
	putStrArr bPrint --Print the board
	putStr $ "\nPlease make your move " ++ stateToStr s ++ ": "
	move <- getNextMove s b
	let board' = makeMove (move, s) b
	let nextTurn = oppState s
	play $ GameState board' nextTurn (points X board') (points O board') . succ $ totalPts env


