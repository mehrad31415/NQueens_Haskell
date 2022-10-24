module Main where

import Data.List

-- The first possible solution to the n queens problem. 
-- https://en.wikipedia.org/wiki/Eight_queens_puzzle


-- a two-dimensional coordinate system will be used for indexing the
-- queens on a chessboard. The coordinates are of the form (row,column). 

-- aliases for defining our embedded domain specific language (EDSL). 
type Row   = Int
type Col   = Int
type Coord = (Row, Col)
type Size  = Int

-- Lists of coordinates of queens will be later used in a Last In
-- First Out (LIFO) manner, so we give this type the alias Stack:
type Candidate = Coord
type Stack     = [Coord]

-- given a coordinate returns the first coordinate of the next row.
nextRow :: Coord -> Coord
nextRow (i,j) = (i+1,1)

-- given a coordinate returns the coordinate of the next column. 
nextCol :: Coord -> Coord
nextCol (i,j) = (i,j+1)

-- the following four functions check that given two coordinates whether they are 
-- on the same row, column, diagonal or antidiagonal respectively. 
sameRow :: Coord -> Coord -> Bool
sameRow (i,j) (k,l) = i == k

sameCol :: Coord -> Coord -> Bool
sameCol (i,j) (k,l) = j == l

sameDiag :: Coord -> Coord -> Bool
sameDiag (i,j) (k,l) = k - i == l - j

sameAntidiag :: Coord -> Coord -> Bool
sameAntidiag (i,j) (k,l) = k - i == j - l

-- this function checks that given a stack of queens and an coordinate whether that
-- coordinate is in danger or not. A coordinate is in danger if it holds the four
-- previous relations with any of the four queens.
danger :: Candidate -> Stack -> Bool
danger x [] = False
danger x (y:ys)
  | sameRow x y || sameCol x y || sameDiag x y || sameAntidiag x y = True
  | otherwise                                                      = danger x ys

-- print the chessboard in a way that given the size and a list of all the queens
-- returns a string. In the string '.' represents an empty space in the chessboard
-- 'Q' represents a space where a queen is placed. Finally, '#' represents a place
-- where it is empty but it is in danger. Theoretically no queen can be placed in that position.
prettyPrint :: Size -> Stack -> String
prettyPrint size xs = concat [prettyPrint' size xs (x,y) | x<- [1..size], y<-[1..size]]

prettyPrint' :: Size -> Stack -> Coord -> String 
prettyPrint' size xs (x,y)
  | (x, y) `elem` xs = if y == size then "Q\n" else "Q"
  | danger (x,y) xs  = if y == size then "#\n" else "#"
  | otherwise        = if y == size then ".\n" else "."

-- Given the size of the chessboard and a stack, the function fixFirst
-- takes the queen on the top of the stack, and if it is in
-- danger, moves it right until it is not in danger.
-- If no safe spot is found for the queen on that row, fixFirst returns Nothing.
fixFirst :: Size -> Stack -> Maybe Stack
fixFirst n ((x,y):ys) 
  | y > n           = Nothing
  | danger (x,y) ys = fixFirst n (nextCol (x,y):ys)
  | otherwise       = Just ((x,y):ys)

-- continue pushes a new candidate to the top of the stack. 
-- The new candidate is at the beginning of the next row.
continue :: Stack -> Stack
continue (y:ys) = nextRow y:y:ys

-- backtrack removes the top element of the stack, and adjusts the 
-- new top element so that it is in the next column.
backtrack :: Stack -> Stack
backtrack (x:y:ys) = nextCol y:ys

-- one step of the greedy algorithm. 
step :: Size -> Stack -> Stack
step size xs = step' size xs (fixFirst size xs)

step' :: Size -> Stack -> Maybe Stack -> Stack 
step' size xs Nothing       = backtrack xs
step' size (x:xs) (Just ys) = continue ys

-- given a size and a stack runs the greedy algorithm function (step function)
-- until it returns a viable stack.
finish :: Size -> Stack -> Stack
finish size (y:ys) 
  | length (y:ys) == size+1 = ys 
  | otherwise               = finish size (step size (y:ys))


-- solves the n queen problem placing a queen in position (1,1).
solve :: Size -> Stack
solve n = finish n [(1,1)]

main :: IO ()
main = do
    print "for what n? "
    n <- getLine
    print "The n queen problem solution is :"
    putStrLn (prettyPrint (read n) (solve (read n)))