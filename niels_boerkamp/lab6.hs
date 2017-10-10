module Lab6 where

import Lecture6 hiding (composites)
import System.Random
import Data.List

-- Exercise 1 
-- Time spent: 20 minutes
-- See lecture6

-- Exercise 3
-- Time spent:
composites :: [Integer]
composites = [x | x <- [0..], length (factors x) >= 2]

-- Exercise 3 Test
exerciseThreeTest = do
                 (takeWhile (<= 150) Lab6.composites) == [4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20, 21, 22, 24, 25, 26, 27, 28, 30, 32, 33, 34, 35, 36, 38, 39, 40, 42, 44, 45, 46, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 60, 62, 63, 64, 65, 66, 68, 69, 70, 72, 74, 75, 76, 77, 78, 80, 81, 82, 84, 85, 86, 87, 88, 90, 91, 92, 93, 94, 95, 96, 98, 99, 100, 102, 104, 105, 106, 108, 110, 111, 112, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 128, 129, 130, 132, 133, 134, 135, 136, 138, 140, 141, 142, 143, 144, 145, 146, 147, 148, 150]
                 
-- Exercise 4
-- Time spent: 45 minutes

primalityCheck f (x:xs) = do
     res <- f x
     if res then print x else return ()
     primalityCheck f xs
     

exerciseFour = primalityCheck (primeTestsF 1) composites
-- After running exerciseFour a couple of times I found
-- that 9 is the smallest composites which could pass this test.
-- *Lab6> exerciseFour
-- 9
-- 45
-- 55
-- 105
-- 208
-- 231
-- 259
-- 341
-- 561
-- 589
-- 671

-- Exercise 5
-- Time spent: 
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1)]

exerciseFive = primalityCheck (primeTestsF 1) carmichael

-- exerciseFive gave me the following output:
-- *Lab6> exerciseFive
-- 294409

-- A Carmichael number will pass a Fermat primality test 
-- to every base b relatively prime to the number, even though it is not actually prime.

-- Exerise 6a
exerciseSixA = primalityCheck (primeMR 1) carmichael

-- Exercise 6b
exerciseSixB =  primalityCheck (primeMR 1) [2^x-1 | x <- primes]



