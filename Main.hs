module Main where

import Control.Monad

import Board

main :: IO ()
main = do
	pp (makeMove m1 b1)

{-- Temporary constants --}
b1 :: Board
b1 = othelloBoard
b2 = flipMult [(2,6)] X (flipMult [(2,4),(2,5)] O b1)

p1 :: Point
p1 = (2, 3)

m1 :: Move
m1 = (p1, X)


