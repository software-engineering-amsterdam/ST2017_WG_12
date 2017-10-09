import Data.List
import Data.Maybe
import Lecture5

genProblemBlocks :: Node -> IO Node
genProblemBlocks n = do
                    n1 <- getRandomInt (length xs)
                    let r1 = bl (fst (xs !! n1))
                    let c1 = bl (snd (xs !! n1))
                    [eraseN (n !! 0) (r,c) | r <- r1, c <- c1]
                    ys <- randomize xs
                    return (minimalize n ys)
   where xs = filledPositions (fst n)

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblemBlocks r
          showNode s