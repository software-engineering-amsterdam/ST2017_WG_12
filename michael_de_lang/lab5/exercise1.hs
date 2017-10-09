import Data.List
import Lecture5
                  
extraBlocks :: (Row, Column) -> [(Row, Column)]
extraBlocks (r,c) | r >= 2 && r <= 4 && c >= 2 && c <= 4 = [(r',c') | r' <- [2..4], c' <- [2..4]]
                  | r >= 2 && r <= 4 && c >= 6 && c <= 8 = [(r',c') | r' <- [2..4], c' <- [6..8]]
                  | r >= 6 && r <= 8 && c >= 2 && c <= 4 = [(r',c') | r' <- [6..8], c' <- [2..4]]
                  | r >= 6 && r <= 8 && c >= 6 && c <= 8 = [(r',c') | r' <- [6..8], c' <- [6..8]]
                  | otherwise = []

subExtraGrid :: Sudoku -> (Row,Column) -> [Value]
subExtraGrid s (r,c) = 
  [ s (r',c') | (r', c') <- extraBlocks (r,c) ]

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
sameblock' (r,c) (x,y) = (bl r == bl x && bl c == bl y) || (extra1 /= [] && extra2 /= [] && extra1 == extra2)
    where extra1 = extraBlocks (r,c)
          extra2 = extraBlocks (x,y)
         
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
   
main = do
    solveAndShow' problemGrid
    solveAndShow' problemGrid
    solveAndShow' problemGrid
    solveAndShow' problemGrid

-- time taken: 3h