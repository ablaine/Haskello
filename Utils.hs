module Utils where

import Data.Array

import DataTypes

addPair :: Point -> Point -> Point
addPair (a,b) (c,d) = (a+c, b+d)

mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
mkArray f bnds = array bnds [(i, f i) | i <- range bnds]

oppState :: State -> State
oppState X = O
oppState O = X
oppState _ = error "oppState called on E (Empty)!"


prettyPrint :: Board -> IO ()
prettyPrint board = do
	putStrLn "  0 1 2 3 4 5 6 7"
	mapM_ (\(a,b) -> rowToStr a b) $ zip (indices board) (elems board)
	where
		rowToStr index row = do
			putStr $ show index
			mapM_ putStr $ map (\a -> if a =="E" then " ." else " " ++ a) (map show $ elems row)
			putStr "\n"

pp = prettyPrint

{- TODO: Use this...? -}
stateToStr :: State -> String
stateToStr X = "X"
stateToStr O = "O"
stateToStr E = "."


