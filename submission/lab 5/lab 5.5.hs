module Lab5 where

import Lecture5
import Data.List

type Position = (Row, Column)
type Constrnt = [[Position]]

-- Exercise 5
-- Time spent: 
-- Michael:     0m - incomplete
-- Constantijn: 0m - incomplete
-- Arjan:       0m - incomplete
-- Niels:       30m

constraints' :: Sudoku -> [Constraint] 
constraints' s = sortBy length3rd 
    [(r,c, freeAtPos' s (r,c) allConstrnt) | 
                       (r,c) <- openPositions s ]

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
  | sameblock' (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune' (r,c,v) rest
  | otherwise = (x,y,zs) : prune' (r,c,v) rest          
         
rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
nrcConstrnt = [subblock [2..4] [2..4]] ++ [subblock [6..8] [6..8]] 
                   ++ [subblock [2..4] [6..8]] ++ [subblock [6..8] [2..4]]

allConstrnt = rowConstrnt ++ columnConstrnt ++ blockConstrnt ++ nrcConstrnt          

nrcBlocks = [[2..4], [6..8]]

sameblock' :: (Row,Column) -> (Row,Column) -> Bool
sameblock' (r,c) (x,y) = (bl r == bl x && bl c == bl y) || (nrcBl r == nrcBl x && nrcBl c == nrcBl y)

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let ys = filter (elem (r,c)) xs 
                        in foldl1 intersect (map ((values \\) . map s) ys)

subblock :: [Int] -> [Int] -> [(Int, Int)]
subblock xs ys = [(r,c)| r <- xs, c <- ys]

nrcBl :: Int -> [Int]
nrcBl x = concat $ filter (elem x) nrcBlocks  
         
emptyNrc :: Node
emptyNrc = (\ _ -> 0, constraints' (\ _ -> 0))

rsolveNs' :: [Node] -> IO [Node]
rsolveNs' ns = rsearch' rsuccNode' solved (return ns)

rsearch' :: (node -> IO [node]) -> (node -> Bool) -> IO [node] -> IO [node]
rsearch' succ goal ionodes = 
  do xs <- ionodes 
     if null xs 
       then return []
       else 
         if goal (head xs) 
           then return [head xs]
           else do ys <- rsearch' succ goal (succ (head xs))
                   if (not . null) ys 
                      then return [head ys]
                      else if null (tail xs) then return []
                           else 
                             rsearch' succ goal (return $ tail xs)

rsuccNode' :: Node -> IO [Node]
rsuccNode' (s,cs) = do xs <- getRandomCnstr cs
                       if null xs 
                         then return []
                         else return (extendNode' (s,cs\\xs) (head xs))
                             
exerciseFive = do [r] <- rsolveNs' [emptyNrc]
                  showNode r
                  s  <- genProblem r
                  showNode s