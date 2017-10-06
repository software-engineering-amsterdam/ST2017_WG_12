module Lab5

where

import Data.List
import System.Random
import Lecture5 hiding (main)

-- Exercise 4 (30 minutes)

genEmptyBlocksProblem :: Node -> IO Node
genEmptyBlocksProblem n = do
    y <- randomize [(1,1),(1,4),(1,7),(4,1),(4,4),(4,7),(7,1),(7,4),(7,7)]
    let xs = take 3 $ y
    let eb = [(r,c) | x <- xs, r <- (bl $ fst x), c <- (bl $ snd x)]
    return (emptyPositions n eb)

emptyPositions :: Node -> [(Row,Column)] -> Node
emptyPositions n [] = n
emptyPositions n (h:t) = emptyPositions (eraseN n h) t

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genEmptyBlocksProblem r
          showNode s
          showNode $ head $ solveNs [s]
