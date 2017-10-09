module Lab5 where

import Data.List
import Data.Maybe
import Lecture5

-- Exercise 3
-- Time spent: 
-- Michael:     2h
-- Constantijn: 3h
-- Arjan:       5h
-- Niels:       15m - incomplete


isMinimal' :: Sudoku -> [(Row, Column)] -> (Bool, Maybe Sudoku)
isMinimal' s [] = (True, Nothing)
isMinimal' s (p:ps) = if (possibleSolutions /= 1) then
                        isMinimal' s ps
                      else
                        (False, Just s')
    where s' = extend s (p,0)
          possibleSolutions = length $ take 2 $ solveNs [(s',constraints s')]

-- returns whether a given grid is in minimal form
-- if not, returns false and the solution which is more-minimal, but not necessarily minimal.
isMinimal :: Grid -> (Bool, Maybe Sudoku)
isMinimal g = if solvable then
                (isMinimal' s $ filledPositions s)
              else
                (False, Nothing)
            where s = grid2sud g
                  solvable = (length $ take 2 $ solveNs [(s,constraints s)]) == 1

testMinimal :: Grid -> IO Bool
testMinimal g = if not (fst res)
                    then do
                        print "original: "
                        showGrid g
                        let s = snd res
                        if (isJust s)
                            then do 
                                print "improvement: "
                                (showSudoku (fromJust s))
                                return False
                            else do
                                print "Non-solvable sudoku"
                                return False
                else do
                    print "Minimal sudoku"
                    return True
                where res = isMinimal g

-- from https://commons.wikimedia.org/w/index.php?title=File:Oceans_Sudoku17_Puzzle-39451_trimmed.png&oldid=242383480
minimalSudoku = [[0,0,0,0,0,0,0,1,0],
              [0,0,0,0,0,2,0,0,3],
              [0,0,0,4,0,0,0,0,0],
              [0,0,0,0,0,0,5,0,0],
              [4,0,1,6,0,0,0,0,0],
              [0,0,7,1,0,0,0,0,0],
              [0,5,0,0,0,0,2,0,0],
              [0,0,0,0,8,0,0,4,0],
              [0,3,0,9,1,0,0,0,0]]
    
exerciseThree = do
    print "example 1"
    testMinimal example1
    print "example 2"
    testMinimal example2
    print "example 3"
    testMinimal example3
    print "example 4"
    testMinimal example4
    print "example 5"
    testMinimal example5
    print "minimalSudoku"
    print "We have found a very difficult minimalSudoku for our function to test it with. It might take a while."
    testMinimal minimalSudoku

{-
output of program: "example 1"
"original: "
+-------+-------+-------+
| 5 3   |   7   |       |
| 6     | 1 9 5 |       |
|   9 8 |       |   6   |
+-------+-------+-------+
| 8     |   6   |     3 |
| 4     | 8   3 |     1 |
| 7     |   2   |     6 |
+-------+-------+-------+
|   6   |       | 2 8   |
|       | 4 1 9 |     5 |
|       |   8   |   7 9 |
+-------+-------+-------+
"improvement: "
+-------+-------+-------+
|   3   |   7   |       |
| 6     | 1 9 5 |       |
|   9 8 |       |   6   |
+-------+-------+-------+
| 8     |   6   |     3 |
| 4     | 8   3 |     1 |
| 7     |   2   |     6 |
+-------+-------+-------+
|   6   |       | 2 8   |
|       | 4 1 9 |     5 |
|       |   8   |   7 9 |
+-------+-------+-------+
"example 2"
"original: "
+-------+-------+-------+
|   3   |   7   |       |
| 6     | 1 9 5 |       |
|   9 8 |       |   6   |
+-------+-------+-------+
| 8     |   6   |     3 |
| 4     | 8   3 |     1 |
| 7     |   2   |     6 |
+-------+-------+-------+
|   6   |       | 2 8   |
|       | 4 1 9 |     5 |
|       |   8   |   7 9 |
+-------+-------+-------+
"improvement: "
+-------+-------+-------+
|   3   |       |       |
| 6     | 1 9 5 |       |
|   9 8 |       |   6   |
+-------+-------+-------+
| 8     |   6   |     3 |
| 4     | 8   3 |     1 |
| 7     |   2   |     6 |
+-------+-------+-------+
|   6   |       | 2 8   |
|       | 4 1 9 |     5 |
|       |   8   |   7 9 |
+-------+-------+-------+
"example 3"
"Minimal sudoku"
"example 4"
"original: "
+-------+-------+-------+
| 1 2 3 | 4 5 6 | 7 8 9 |
| 2     |       |       |
| 3     |       |       |
+-------+-------+-------+
| 4     |       |       |
| 5     |       |       |
| 6     |       |       |
+-------+-------+-------+
| 7     |       |       |
| 8     |       |       |
| 9     |       |       |
+-------+-------+-------+
"Non-solvable sudoku"
"example 5"
"original: "
+-------+-------+-------+
| 1     |       |       |
|   2   |       |       |
|     3 |       |       |
+-------+-------+-------+
|       | 4     |       |
|       |   5   |       |
|       |     6 |       |
+-------+-------+-------+
|       |       | 7     |
|       |       |   8   |
|       |       |     9 |
+-------+-------+-------+
"Non-solvable sudoku"
"minimalSudoku"
"Minimal sudoku"
-}