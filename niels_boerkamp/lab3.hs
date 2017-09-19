module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Exercise 1
-- 60 minutes

contradiction, tautology :: Form -> Bool
contradiction f = and (map (\v -> not (evl v f)) (allVals f))
tautology f = and (map (\v -> evl v f) (allVals f))

-- logical entailment
equiv, entails :: Form -> Form -> Bool
equiv p q = and (map (\v -> (evl v p) == (evl v q)) (allVals p))
entails a b = and ( map (\v -> evl (v ++ (fillVals b a)) b) (filter (\v -> evl v a) (allVals a)))
              where fillVals a b = map (\x -> (x,False)) (filter (\x -> not (elem x (propNames b))) (propNames a))

f1 = Cnj [p, q]
f2 = Dsj [q, p]

exerciseOne = do
    print $ equiv f1 f2
    print $ equiv f1 f1

-- Testcases
-- equiv f1 f2 == False
-- equiv f1 p == False
-- equiv f1 f1 == True
-- equiv f2 f2 == True

-- Exercise 2
-- xx minutes

-- Test 1:  parse "*(1 +(2 -3)"
-- Testresult: []
-- This test fails since the parentheses aren't used correctly. In this case
-- they are opend twice and only closed once.
exerciseTwoTestOne = do
         parse "*(1 +(2 -3)" == []


-- Exercise 4
-- Inspiration for random function: https://www.vex.net/~trebla/haskell/random.xhtml
-- This function generates a formula based on a list of lists of Integers. Every element of this
-- list of intgers should always contain three elements. 
-- 0: Integer which determines which opperator will be used.         (1-5)
-- 1: Integer which represents a variable							 (0-n)
-- 2: This Integer determines if the 'tree' should go left or right. (0-1)

formulaGenerator :: [[Int]] -> String -> String
formulaGenerator (y:ys) x | y !! 0 == 1 && y !! 2 == 0 = formulaGenerator ys ("*(" ++ show (y !! 1) ++ " " ++ x ++ ")")
                          | y !! 0 == 1 && y !! 2 == 1 = formulaGenerator ys ("*(" ++ x ++ " " ++ show (y !! 1) ++ ")")

                          | y !! 0 == 2 && y !! 2 == 0 = formulaGenerator ys ("+(" ++ x ++ " " ++ show (y !! 1) ++ ")")
                          | y !! 0 == 2 && y !! 2 == 1 = formulaGenerator ys ("+(" ++ show (y !! 1) ++ " " ++ x ++ ")")

                          | y !! 0 == 3 = formulaGenerator ys ("-" ++ x )

                          | y !! 0 == 4 && y !! 2 == 0 = formulaGenerator ys ("("++ show (y !! 1) ++ " ==> " ++ x ++ ")")
                          | y !! 0 == 4 && y !! 1 == 0 = formulaGenerator ys ("("++ x ++ " ==> " ++ show (y !! 1) ++ ")")

                          | y !! 0 == 5 && y !! 2 == 0 = formulaGenerator ys ("(" ++ x ++ " <=> " ++ show (y !! 1) ++")")
                          | y !! 0 == 5 && y !! 1 == 0 = formulaGenerator ys ("(" ++ show (y !! 1) ++" <=> " ++ x ++ ")")
 
                          | otherwise = formulaGenerator ys x
formulaGenerator [] x = x

-- This functions generates a random sequence of Ints
randomSequenceN :: Int -> Int -> Int -> IO [Int]
randomSequenceN n lower upper = sequence (replicate n (randomRIO (lower,upper)))

toTuples :: [Int] -> [Int] -> [Int] -> [[Int]]
toTuples xs ys zs = zipWith3 (\ x y z -> [x, y, z]) xs ys zs

exerciseFour = do let n = 50
                  opp <- randomSequenceN n 1 5
                  vars <- randomSequenceN n 0 10
                  coins <- randomSequenceN n 0 1
                  print $ formulaGenerator (toTuples opp vars coins) "0"
                  print ( "PARSED  " ++ show (parse (formulaGenerator (toTuples opp vars coins) "0")))