module Utils where

import System.Cmd
import Data.Array

import DataTypes

addPair :: Point -> Point -> Point
addPair (a,b) (c,d) = (a+c, b+d)

mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
mkArray f bnds = array bnds [(i, f i) | i <- range bnds]

-- Converts a 2D Array into a 2D list of it's elements.
elems2D :: (Ix i, Ix a) => Array a (Array i e) -> [[e]]
elems2D arr = map (\i -> elems $ arr ! i) . range $ bounds arr

-- Performs 'map' over an array. Function expects the tuple (index,value) but should only return the new value.
mapArray :: (Ix i) => ((i, a) -> b) -> Array i a -> Array i b
mapArray f arr = array (bounds arr) $ map (\(index,value) -> (index,f (index,value))) $ assocs arr

-- Similar to mapArray except performed on a 2d array of arrays. Function expects ((x,y),value) but you should only return the new value.
mapArray2D :: (Ix i) => (((i, i), a) -> b) -> Array i (Array i a) -> Array i (Array i b)
mapArray2D f arr = array (bounds arr) $ map (\(x,rArr) -> (x,handleRow (x,rArr))) $ assocs arr
	where
		handleRow (x,rArr) = array (bounds rArr) $ map (\(y,value) -> (y,f ((x,y),value))) $ assocs rArr

{- State -}
oppState :: State -> State
oppState X = O
oppState O = X
oppState _ = error "oppState called on E (Empty)!"

stateToStr :: State -> String
stateToStr X = "Dark"
stateToStr O = "Light"
stateToStr E = "Empty"

{- Board representations -}
boardToArr :: Board -> [String]
boardToArr = map handleRow . elems2D
	where
		handleRow = concatMap (\i -> if i == E then " ." else if i == O then o else x)
			where
				x = " " ++ xColor ++ "x" ++ clear
				o = " " ++ oColor ++ "o" ++ clear

showBoard :: Board -> [String]
showBoard = prettifyBoard . boardToArr
	where
		prettifyBoard :: [String] -> [String]
		prettifyBoard lst = "  0 1 2 3 4 5 6 7" : zipWith (++) (map show [0..7]) lst

putStrArr :: [String] -> IO ()
putStrArr = mapM_ (\item -> system $ "echo '" ++ item ++ "'")

printBoard :: Board -> IO ()
printBoard = putStrArr . showBoard

printRequestToMakeMove state = system $ "echo '" ++ "\nPlease make your move " ++ player ++ ": \\c'"
	where
		player = if state == X then xColor ++ stateToStr state ++ clear else oColor ++ stateToStr state ++ clear

--error when I try to declare type
printScore x o = system $ "echo '     " ++ xScore ++ spacing ++ oScore ++ "'"
	where
		xScore = xColor ++ show x ++ clear
		oScore = oColor ++ show o ++ clear
		spacing = "     " ++ if x >= 10 then "" else " "

{- Edit these to one of the below to choose player colors. -}
xColor = red
oColor = green

{- Constants -}
green = "\\033[0;32m"
red = "\\033[0;31m"
yellow = "\\033[0;33m"
blue = "\\033[0;34m"
clear = "\\033[0m"
