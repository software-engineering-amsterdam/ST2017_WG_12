module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Control.Monad
import Lecture3

import Debug.Trace

-- Exercise 1 (60 minutes)

contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails p q = and (map (\ v -> evl v q) (filter (\ v -> evl v p) (allVals p)))

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv p q = l && r
    where
        l = and (map (\ v -> (evl v p) == (evl v q)) (allVals p))
        r = and (map (\ v -> (evl v p) == (evl v q)) (allVals q))

-- Exercise 2 (5 minutes)

-- Exercise 3 (130 minutes)
cnf :: Form -> Form
cnf (Prop p) = Prop p

cnf (Cnj [p, q]) = Cnj [cnf p, cnf q]
cnf (Dsj [p, q]) = Dsj [cnf p, cnf q]

cnf (Neg (Neg p)) = cnf p
cnf (Neg (Cnj [p, q])) = Dsj [cnf (Neg p), cnf (Neg q)]
cnf (Neg (Dsj [p, q])) = Cnj [cnf (Neg p), cnf (Neg q)]

cnf (Neg (Impl p q)) = cnf (Neg (Dsj [Neg p, q]))
cnf (Neg (Equiv p q)) = cnf (Neg (Dsj [Cnj [p, q], Cnj [Neg p, Neg q]]))

cnf (Neg p) = Neg (cnf p)

cnf (Impl p q) = cnf (Dsj [Neg p, q])
cnf (Equiv p q) = cnf (Dsj [Cnj [p, q], Cnj [Neg p, Neg q]])

cnfx :: Form -> Form
cnfx = nnf . arrowfree

isCNF, isClause, isLiteral :: Form -> Bool
isCNF (Cnj [p, q]) = isClause p && isCNF q
isCNF f = isClause f

isClause (Dsj [p, q]) = isLiteral p && isClause q
isClause f = isLiteral f

isLiteral (Prop _) = True
isLiteral (Neg (Prop _)) = True
isLiteral f = False

-- Exercise 4 (50 minutes)
formula :: Gen Form
formula = resize 3 $ sized formula'

formula' :: Int -> Gen Form
formula' 0 = liftM Prop arbitrary
formula' n | n > 0 =
    oneof [liftM Neg subformula1,
        --    liftM Prop arbitrary -- this makes n not correct
           liftM Cnj (vectorOf 2 subformula2),
           liftM Dsj (vectorOf 2 subformula2),
           liftM2 Impl subformula2 subformula2,
           liftM2 Equiv subformula2 subformula2]
    where
        subformula1 = formula' (n - 1)
        subformula2 = formula' (div n 2) -- TODO needs more random

-- quickCheck $ forAll formula (\ f -> isCNF (cnf f))
-- forAll formula (\ f -> (nnf $ arrowfree f) == (cnf f))

-- +(+(3 0) (2==>-1))

-- formulaCNF =

-- data RandomForm = RandomForm Form deriving Show
--
-- instance Arbitrary RandomForm where
--     arbitrary = fmap RandomForm (listOf $ elements (map chr [0..127]))
