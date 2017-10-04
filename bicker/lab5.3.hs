module Lab5

where

import Data.List
import System.Random
import Lecture5

-- Exercise 3 (180 minutes)

minimal :: Grid -> Bool
minimal gr = (f gr == 1) && ((length $ filter (< 2) $ fmap f p') == 0)
    where
        s = grid2sud gr
        p' = fmap sud2grid [eraseS s (r,c) | r <- values, c <- values, s (r,c) /= 0 ]
        f = (\ gr -> length $ take 2 $ solveNs $ initNode gr)

minimalSudoku :: Grid
minimalSudoku = [[0,0,0,0,0,0,0,1,0],
                 [0,0,0,0,0,2,0,0,3],
                 [0,0,0,4,0,0,0,0,0],
                 [0,0,0,0,0,0,5,0,0],
                 [4,0,1,6,0,0,0,0,0],
                 [0,0,7,1,0,0,0,0,0],
                 [0,5,0,0,0,0,2,0,0],
                 [0,0,0,0,8,0,0,4,0],
                 [0,3,0,9,1,0,0,0,0]]
