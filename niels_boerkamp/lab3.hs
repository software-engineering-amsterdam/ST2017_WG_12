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
