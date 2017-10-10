module Lab6

where

import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Lecture6 hiding (exM, composites)

-- Exercise 1 (15 minutes)

exM :: Integer -> Integer -> Integer -> Integer
exM b e m = exM' b e 0 m 1

exM' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
exM' b e e' m c
    | e' < e = exM' b e (e' + 1) m (b * c `mod` m)
    | otherwise = c

-- Exercise 2 ()

-- Exercise 3 (15 minutes)

composites :: [Integer]
composites = [x | x <- [1..], (length $ factors x) > 1]

-- Exercise 4 (180 minutes)
primalityTest :: Show a => (a -> IO Bool) -> [a] -> IO ()
primalityTest _ [] = print "Done."
primalityTest f (x:xs) = do
    r <- f x
    if r then print x else return ()
    primalityTest f xs

-- k = 1, lowest composite found 9
-- k = 2, lowest composite found 9
-- k = 3, lowest composite found 45

exerciseFour k = do
    primalityTest (primeTestsF k) composites

-- Exercise 5 (15 minutes)
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
    k <- [2..],
    prime (6*k+1),
    prime (12*k+1),
    prime (18*k+1) ]

exerciseFive k = do
    primalityTest (primeTestsF k) carmichael

-- Exercise 6 (10 minutes)
exerciseSix k = do
    primalityTest (primeMR k) carmichael

-- Exercise 7 (10 minutes)
exerciseSeven k = do
    -- primalityTest (primeMR k) (fmap (\p -> 2 ^ p - 1) primes)
    primalityTest (primeMR k) [mers x | x <- [1..25]]
