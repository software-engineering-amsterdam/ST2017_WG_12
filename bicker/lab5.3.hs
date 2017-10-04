module Lab5

where

import Data.List
import System.Random
import Lecture5

-- Exercise 3 (180 minutes)

-- minimal :: Grid -> Bool
-- minimal gr = uniqueSol n && ((length $ filter (< 2) $ fmap f p') == 0)
--     where
--         n = head $ initNode gr
--         s = grid2sud gr
--         p' = fmap sud2grid [eraseS s (r,c) | r <- values, c <- values, s (r,c) /= 0 ]
--         f = (\ gr -> length $ take 2 $ solveNs $ initNode gr)

minimal :: Grid -> Bool
minimal gr = (f gr == 1) && ((length $ filter (< 2) $ fmap f p') == 0)
    where
        s = grid2sud gr
        p' = fmap sud2grid [eraseS s (r,c) | r <- values, c <- values, s (r,c) /= 0 ]
        f = (\ gr -> length $ take 2 $ solveNs $ initNode gr)

-- minimal :: Grid -> Bool
-- minimal gr = p && p'
--     where
--         p = f gr == 1
--         p' = (length $ filter (<2) $ fmap f $ gridRemoveHint gr) == 0
--         f = (\ gr -> length $ take 2 $ solveNs $ initNode gr)

-- minimal :: Grid -> Bool
-- minimal gr = minimal' 1 gr

-- minimal' n gr
--     | gr == emptyGrid = True
--     | otherwise = current && next
--     where
--         nsucc = succ n
--         current = n == (length $ take nsucc $ solveNs $ initNode gr)
--         next = and $ fmap (minimal' nsucc) (gridRemoveHint gr)

gridRemoveHint :: Grid -> [Grid]
gridRemoveHint gr = filter (gr /= ) [take r gr ++ [take c (gr!!r) ++ [0] ++ drop (c + 1) (gr!!r)] ++ drop (r + 1) gr | r <- [0..8], c <- [0..8]]

emptyGrid :: Grid
emptyGrid = [[0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0]]

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
