module Utils where

import Data.Array

import DataTypes

addPair :: Point -> Point -> Point
addPair (a,b) (c,d) = (a+c, b+d)

mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
mkArray f bnds = array bnds [(i, f i) | i <- range bnds]

-- Converts a 2D Array into a 2D list of it's elements.
elems2D :: (Ix i, Ix a) => Array a (Array i e) -> [[e]]
elems2D arr = map (\i -> elems $ arr ! i) . range $ bounds arr

-- Allows map like transformations on the elements in the array while leaving the array structure intact
mapArray :: (Ix i) => (a -> b) -> Array i a -> Array i b
mapArray f arr = array (bounds arr) $ map (\(i,e) -> (i,f e)) $ assocs arr

--mapArray2D :: (Ix (Array i a), Ix i) => (a -> b) -> Array (Array i a) e -> Array (Array i a) (Array i b) --NOTE: Compiler complains when type is visible..
mapArray2D f board = array (bounds board) $ map (\(i,e) -> (i,mapArray f i)) $ assocs board

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
		handleRow = concatMap (\i -> if i == E then " ." else if i == O then " o" else " x")

showBoard :: Board -> [String]
showBoard = prettifyBoard . boardToArr
	where
		prettifyBoard :: [String] -> [String]
		prettifyBoard lst = "  0 1 2 3 4 5 6 7" : zipWith (++) (map show [0..7]) lst

putStrArr :: [String] -> IO ()
putStrArr = mapM_ putStrLn

printBoard :: Board -> IO ()
printBoard = putStrArr . showBoard
