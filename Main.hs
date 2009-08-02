module Main where

import System.Cmd
import Control.Concurrent
import Monad

import DataTypes
import Board
import Utils
import Player

main :: IO ()
main = play defaultSettings defaultStart 

{- Demos -}
demo01 = play settings01 defaultStart
demo02 = play settings02 defaultStart
demo03 = play settings03 defaultStart
demo04 = play settings04  defaultStart

{- Game States -}
defaultStart = GameState othelloBoard X 2 2 4

{- Settings -}
settings01 = Settings humanMakeMove		humanMakeMove	0		True
settings02 = Settings randomMakeMove	randomMakeMove	0		False
settings03 = Settings randomMakeMove	randomMakeMove	100000	True
settings04 = Settings humanMakeMove		randomMakeMove	100000	True

defaultSettings = settings01

{- Types -}
type Player = State -> Board -> IO Point

data GameState = GameState
	{ board :: Board
	, curTurn :: State
	, darkPts :: Int
	, lightPts :: Int
	, totalPts :: Int
	} deriving (Show)

data Settings = Settings
	{ dark :: Player
	, light :: Player
	, delay :: Int
	, output :: Bool }

{- Main game loop -}
play env gameState = do
	let b = board gameState
	let s = curTurn gameState
	let s' = oppState s
	if hasValidMove s b
		then do
			refreshScreen $ output env
			requestMove s
		else if hasValidMove s' b
			then do
				refreshScreen $ output env
				when (output env) $ putStrLn $ stateToStr s ++ " was unable to make any move."
				requestMove s'
			else do
				refreshScreen True --Always print the final screen
				putStrLn "Game is over!"
	where
		refreshScreen output = do
			threadDelay $ delay env
			when output $ do
				system "clear"
				putStrLn "<<   Haskello   >>"
				printScore (darkPts gameState) (lightPts gameState)
				printBoard (board gameState)
				putStr "\n"
		requestMove playerState = do
			let b = board gameState
			let s = playerState
			let s' = oppState s
			when (output env) $ printRequestToMakeMove s
			let performMove = (if s == X then dark else light) env
			move <- performMove s b
			let b' = makeMove (move, s) b
			play env $ GameState b' s' (points X b') (points O b') . succ $ totalPts gameState

