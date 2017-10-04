module Lab5

where

import Data.List
import System.Random
import Lecture5 hiding (main)

-- Exercise 4 (30 minutes)

genEmptyBlocksProblem :: Node -> IO Node
genEmptyBlocksProblem n = do
    let eb = [(r,c) | r <- [1..3], c <- [1..9]]
    return (emptyPositions n eb)

emptyPositions :: Node -> [(Row,Column)] -> Node
emptyPositions n [] = n
emptyPositions n (h:t) = emptyPositions (eraseN n h) t

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genEmptyBlocksProblem r
          showNode s
