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
-- Inspiration for random: https://www.vex.net/~trebla/haskell/random.xhtml
-- This generator starts by generating a random sequence of numbers 
-- which is of variable length. After that it will add a variable and operator
-- depending on the random value.
formulaGenerator :: [Int] -> String -> String
formulaGenerator (y:ys) x | y == 1 = formulaGenerator ys ("*(1 " ++ x ++ ")")
                          | y == 2 = formulaGenerator ys ("+(2 " ++ x ++ ")")
                          | y == 3 = formulaGenerator ys ("-" ++ x )
                          | y == 4 = formulaGenerator ys ("(3 ==> " ++ x ++ ")")
                          | y == 5 = formulaGenerator ys ("(3 <=> " ++ x ++ ")")
                          | otherwise = formulaGenerator ys x
formulaGenerator [] x = x

generateFormula = do s <- randomSequence
                     print $ formulaGenerator s "0" 

-- This functions generates a random sequence of Ints
randomSequence :: IO [Int]
randomSequence =
    do n <- randomRIO (1,1000)
       sequence (replicate n (randomRIO (1,5)))

--randomSequenceN :: Int -> Int -> Int -> IO [Int]
--randomSequenceN n lower upper = sequence (replicate n (randomRIO (lower,upper)))


--toTuples :: IO [Int] -> IO [Int] -> IO [(Int, Int)]
--toTuples xs ys = zipWith (\ x y -> (x, y)) xs ys

--randomTuples :: Int -> [(Int, Int)]
--randomTuples n = toTuples opp vars
--                 where opp = randomSequenceN n 0 5
--				       vars = randomSequenceN n 0 3


-- Tests
-- Try to parse the generated formulas.
exerciseFourTests = do s <- randomSequence
                       print $ formulaGenerator s "0" 
                       print $ parse (formulaGenerator s "0")
