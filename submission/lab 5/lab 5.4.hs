module Lab5

where

import Data.List
import System.Random
import Lecture5

-- Exercise 4 
-- Time spent:
-- Michael      1h
-- Constantijn  90m
-- Arjan        10h
-- Niels        2h

-- We have chosen Constantijn's implementation, since he had the best working code. This code might generate
-- a sudoku problem which has multiple solutions.
-- The chance of generation a sudoku problem with multiple solutions increases 
-- when more blocks are left empty. This complexity of this problem increases exponentially.

genEmptyBlocksProblem :: Node -> IO Node
genEmptyBlocksProblem n = do
    y <- randomize [(1,1),(1,4),(1,7),(4,1),(4,4),(4,7),(7,1),(7,4),(7,7)]
    let xs = take 3 $ y
    let eb = [(r,c) | x <- xs, r <- (bl $ fst x), c <- (bl $ snd x)]
    return (emptyPositions n eb)

emptyPositions :: Node -> [(Row,Column)] -> Node
emptyPositions n [] = n
emptyPositions n (h:t) = emptyPositions (eraseN n h) t

exerciseFour :: IO ()
exerciseFour = do 
          [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genEmptyBlocksProblem r
          showNode s
          showNode $ head $ solveNs [s]