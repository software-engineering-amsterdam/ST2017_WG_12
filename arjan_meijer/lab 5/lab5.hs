module Lab5 where

import Lecture5
import Data.List
import Control.Monad
import Debug.Trace

-- Exercise 3
-- Time spend: 300 min

-- We check if the generated sudokus are minimal with brute-force
-- If we iterate through all elements, and make them 0 (empty) 
-- we can check if there are then more than 1 solutions.
-- The disadvantage of brute-force testing is that it can take a while to check.

combinations :: [Integer] -> Integer -> Bool -> [[Integer]]
combinations xs n d | n == 1 = toIntA xs
                    | otherwise = concat (map (\x -> [x:y | y <- (combinations xs (n-1) d), not(elem x y) || d]) xs)

uniqueSet :: [[Integer]] -> [[Integer]]
uniqueSet (x:xs) | y = uniqueSet xs
                 | xs == [] = [x]
                 | otherwise = x:(uniqueSet xs)
                 where y = any (\z -> elem z xs) (permutations x)

toIntA :: [Integer] -> [[Integer]]
toIntA (x:xs) | xs == [] = [[x]]
              | otherwise = [x]:(toIntA xs)


comb :: [Integer] -> [Integer] -> [(Integer,Integer)]
comb xs ys = concat (map (\i -> (map (\j -> (i,j)) ys)) xs)

isMinimal :: Sudoku -> IO Bool
isMinimal s = do 
                let r = map (\x -> do 
                           let v = solveNs( initNode (removeAt (x !! 0, x !! 1) (sud2grid s)))
                           return ((length v) > 1)
                           
                        )(combinations [1..9] 2 True)
                y <- sequence r
                let z = (solveNs (initNode (sud2grid s)))
                return ((and y) && (length z) == 1)

removeAt :: (Integer, Integer) -> [[Value]] -> Grid
removeAt (i, j) (x:xs) | i == 0 = ((minRem j x):xs)
                       | otherwise = (x : (removeAt ((i - 1), j) xs))
removeAt (i, j) [] = trace (show i ++ " - " ++ show j) []

minRem :: Integer -> [Value] -> [Value]
minRem i (x:xs) | i == 0 = (0:xs)
                | otherwise = (x : (minRem (i - 1) xs))
minRem i [] = error "Index out of range"

exerciseThree = do [r] <- rsolveNs [emptyN]
                   s  <- genProblem r
                   y <- (isMinimal (fst s))
                   print y

-- Exercise 4
-- Time spend:
-- 1 2 3
-- 4 5 6
-- 7 8 9

getBlock :: Integer -> [(Integer,Integer)]
getBlock n  | n == 0 = comb [0..2] [0..2]
            | n == 1 = comb [0..2] [3..5]
            | n == 2 = comb [0..2] [6..8]
            | n == 3 = comb [3..5] [0..2]
            | n == 4 = comb [3..5] [3..5]
            | n == 5 = comb [3..5] [6..8]
            | n == 6 = comb [6..8] [0..2]
            | n == 7 = comb [6..8] [3..5]
            | n == 8 = comb [6..8] [6..8]

clearBlock :: Grid -> Integer -> Grid
clearBlock g n = removeMultiple g (getBlock n)

clearBlocks :: Grid -> [Integer] -> Grid
clearBlocks g (x:xs) = clearBlocks (clearBlock g x) xs
clearBlocks g [] = g

removeMultiple :: Grid -> [(Integer, Integer)] -> Grid
removeMultiple g (x:xs) = removeMultiple (removeAt x g) xs
removeMultiple g [] = g

combinationsWRep xs n = filter ((n==).length.nub) $ mapM (const xs) [1..n]

e4 :: Grid -> Integer -> IO Bool
e4 gr i = do 
            x <- iOAny (\x -> do
                       let v = solveNs( initNode (clearBlocks gr x))
                       return (length v > 0)
                    ) (uniqueSet $ combinations [0..8] i False)
            return (x)

iOAny :: (a -> IO Bool) -> [a] -> IO Bool
iOAny f (x:xs) = do
                  y <- f x
                  case y of
                    True -> return (True)
                    False -> iOAny f xs
iOAny f [] = do return (False)

exerciseFour :: Integer -> IO Bool
exerciseFour n = do
                 [r] <- rsolveNs [emptyN]
                 y <- e4 (sud2grid (fst r)) n
                 return True