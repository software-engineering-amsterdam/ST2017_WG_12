module Lab5

where

import Data.List
import System.Random
import Lecture5 hiding (main)

-- Exercise 4 (30 minutes)

-- sample1 xs = do
--   let l = length xs - 1
--   idx <- randomRIO (0, l)
--   return $ xs !! idx
--
-- sample 0 xs = return []
-- sample n xs = do
--   let l = min n (length xs)
--   val <- sample1 xs
--   (:) <$> (pure val) <*> (sample (l-1) (delete val xs))

genEmptyBlocksProblem :: Node -> IO Node
genEmptyBlocksProblem n = do
    -- let xs = take 3 $ randomize [(1,1),(1,4),(1,7),(4,1),(4,4),(4,7),(7,1),(7,4),(7,7)]
    let xs = [(1,1),(4,4),(7,7)]
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
        --   showNode $ head $ solveNs [s]
