module Lab5 where

import Lecture5
import Data.List
import System.Random

type Position = (Row,Column)
type Constrnt = [[Position]]

-- Exercise 2
-- Time spent:
-- Michael:         150m
-- Constantijn:     70m
-- Arjan:           20m - incomplete
-- Niels:           75m

-- time taken: 150m

-- personally I think that since you still have to edit parts of this spaghetti code, the benefit of this refactoring is minimal.
-- Everytime you have a new constraint, you need to alter allConstraints AND sameblock/prune. 
-- 
-- Regarding the performance characteristics, the output is in exercise1.prof and exercise2.prof:
-- refactoring the code leads to a lower memory requirement of 9,5% but has a negligible impact on total time taken.


blocks' :: [[Int]]
blocks' = [[2..4],[6..8]]

bl' :: Int -> [Int]
bl' x = concat $ filter (elem x) blocks'

rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
extraBlockConstrnt = [[(r,c) | r <- rs, c <- cs] | rs <- [[2..4],[6..8]], cs <- [[2..4],[6..8]] ]
allConstraints = rowConstrnt ++ columnConstrnt ++ blockConstrnt ++ extraBlockConstrnt

freeAtPos2 :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos2 s (r,c) xs = let 
   ys = filter (elem (r,c)) xs 
 in 
   foldl1 intersect (map ((values \\) . map s) ys)
   


constraints2 :: Sudoku -> [Constraint] 
constraints2 s = sortBy length3rd 
    [(r,c, freeAtPos2 s (r,c) allConstraints) | 
                       (r,c) <- openPositions s ]

initNode2 :: Grid -> [Node]
initNode2 gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints2 s)]
              
extendNode2 :: Node -> Constraint -> [Node]
extendNode2 (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune2 (r,c,v) constraints) | v <- vs ]
         
prune2 :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune2 _ [] = []
prune2 (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune2 (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune2 (r,c,v) rest
  | sameblock2 (r,c) (x,y) = (x,y,zs\\[v]) : prune2 (r,c,v) rest
  | otherwise = (x,y,zs) : prune2 (r,c,v) rest

sameblock2 :: (Row,Column) -> (Row,Column) -> Bool
sameblock2 (r,c) (x,y) = (bl r == bl x && bl c == bl y) || (bl' r == bl' x && bl' c == bl' y)
         
succNode2 :: Node -> [Node]
succNode2 (s,[]) = []
succNode2 (s,p:ps) = extendNode2 (s,ps) p 

solveNs2 :: [Node] -> [Node]
solveNs2 = search succNode2 solved 

solveAndShow2 :: Grid -> IO[()]
solveAndShow2 gr = solveShowNs2 (initNode2 gr)

solveShowNs2 :: [Node] -> IO[()]
solveShowNs2 = sequence . fmap showNode . solveNs2

problemGrid :: Grid
problemGrid = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]]
              
exerciseTwo = do
    solveAndShow2 problemGrid
