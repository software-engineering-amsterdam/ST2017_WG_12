module Lab5 where

import Lecture5
import Data.List

-- Exercise 1
-- Time spent:
-- Michael:         3h
-- Constantijn:     1h
-- Arjan:           30m - incomplete
-- Niels:           1h - incomplete

-- For this exercise we have chosen the implementation of Michael, but we 
-- have used some pieces of Constantijn to improve Michael's implementation.

blocks' :: [[Int]]
blocks' = [[2..4],[6..8]]

bl' :: Int -> [Int]
bl' x = concat $ filter (elem x) blocks'

subExtraGrid :: Sudoku -> (Row,Column) -> [Value]
subExtraGrid s (r,c) = [ s (r',c') | r' <- bl' r, c' <- bl' c ]

freeInExtraSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInExtraSubgrid s (r,c) = freeInSeq (subExtraGrid s (r,c))

freeAtPos' :: Sudoku -> (Row,Column) -> [Value]
freeAtPos' s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c)
   `intersect` (freeInSubgrid s (r,c))
   `intersect` (freeInExtraSubgrid s (r, c)) 
   
constraints' :: Sudoku -> [Constraint] 
constraints' s = sortBy length3rd 
    [(r,c, freeAtPos' s (r,c)) | 
                       (r,c) <- openPositions s ]

extragridInjective :: Sudoku -> (Row,Column) -> Bool
extragridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subExtraGrid s (r,c))
                       
consistent' :: Sudoku -> Bool
consistent' s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ extragridInjective s (r,c) |
                     rs <- [[2..4],[6..8]], cs <- [[2..4],[6..8]], r <- rs, c <- cs ]
   
initNode' :: Grid -> [Node]
initNode' gr = let s = grid2sud gr in 
              if (not . consistent') s then [] 
              else [(s, constraints' s)]
         

extendNode' :: Node -> Constraint -> [Node]
extendNode' (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune' (r,c,v) constraints) | v <- vs ]

prune' :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune' _ [] = []
prune' (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | sameblock' (r,c) (x,y) = (x,y,zs\\[v]) : prune' (r,c,v) rest
  | otherwise = (x,y,zs) : prune' (r,c,v) rest

sameblock' :: (Row,Column) -> (Row,Column) -> Bool
sameblock' (r,c) (x,y) = (bl r == bl x && bl c == bl y) || (bl' r == bl' x && bl' c == bl' y)
         
succNode' :: Node -> [Node]
succNode' (s,[]) = []
succNode' (s,p:ps) = extendNode' (s,ps) p 

solveNs' :: [Node] -> [Node]
solveNs' = search succNode' solved 

solveAndShow' :: Grid -> IO[()]
solveAndShow' gr = solveShowNs' (initNode' gr)

solveShowNs' :: [Node] -> IO[()]
solveShowNs' = sequence . fmap showNode . solveNs'

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
   
exerciseOne = do
    solveAndShow' problemGrid