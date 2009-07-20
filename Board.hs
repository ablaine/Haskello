module Board
( State(..), Board, Point, Move
, board_bounds
, board_size
, othelloBoard
, makeMove
) where

import Data.Array

data State = X | O | E
	deriving (Eq, Enum, Show, Bounded)

type Board = Array Int (Array Int State)
type Point = (Int, Int)
type Move = (Point, State)

{-- Constants --}
board_bounds = (0, 7)
board_size = 8

othelloBoard :: Board
othelloBoard = flipMult [(3,3),(4,4)] O (flipMult [(3,4),(4,3)] X blankBoard)
	where
		blankBoard = mkArray (\_ -> row) board_bounds
			where
				row = mkArray (\_ -> E) board_bounds


{-- Manipulating the board --}
makeMove :: Move -> Board -> Board
makeMove m@(_,s) b = flipMult (getFlipped m b) s b

getFlipped :: Move -> Board -> [Point]
getFlipped m@(p,s) b = if null result then [] else p:result
	where
		result = concat (map followInit (listOfValidDir m b))
			where
				followInit d = follow (addPoints p d) []
					where
						follow p lst
							| withinBounds p && getState p b == (oppState s) = follow (addPoints p d) (p:lst)
  							| getState p b == s = lst
							| otherwise = []

listOfValidDir :: Move -> Board -> [Point]
listOfValidDir (p,s) b = filter isValidDir listOfDir
	where
		listOfDir = [ (a,b) | a <- [-1,0,1], b <- [-1,0,1], a /= 0 || b /= 0 ]
		isValidDir d = withinBounds p' && getState p' b == (oppState s)
			where
				p' = addPoints p d

flipMult :: [Point] -> State -> Board -> Board
flipMult [] _ b = b
flipMult (x:xs) val b = flipMult xs val (flipSingle x val b)
	where
		flipSingle (x, y) val b = (b // [(y,((b ! y) // [(x,val)]))])

withinBounds :: Point -> Bool
withinBounds (x, y) = check x && check y
	where
		check = (\n -> 0 <= n && n < board_size)

getState :: Point -> Board -> State
getState (x, y) b = b ! y ! x

{-- Utilities --}
mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
mkArray f bnds = array bnds [(i, f i) | i <- range bnds]

addPoints :: Point -> Point -> Point
addPoints = (\(a,b) (c,d) -> (a+c, b+d))

oppState :: State -> State
oppState X = O
oppState O = X
oppState _ = error "oppState called on E (Empty)!"

b1 = othelloBoard

pp :: Board -> IO ()
pp board = do putStr "  0 1 2 3 4 5 6 7\n"; mapM_ (\(a,b) -> rowToStr a b) (zip (indices board) (elems board))
	where
		rowToStr index row = do putStr (show index); mapM_ putStr str; putStr "\n"
			where
				str = map (\a -> if a == "E" then " ." else " " ++ a) (map show (elems row))


main :: IO ()
main = do
	pp othelloBoard;
	putStr "Dark: Please make your move\n";
	move <- getLine;
	pp (hmm (map read (words move)::[Int]));
	putStr "Done!\n"

hmm lst = makeMove (((lst !! 0),(lst !! 1)), X) othelloBoard;
