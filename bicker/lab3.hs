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
equiv p q = all (\ v -> evl v p) (allVals p) == all (\ v -> evl v q) (allVals q)

-- Exercise 2 (5 minutes)

-- Exercise 3 (100 minutes)
toCNF :: Form -> Form
toCNF (Impl p q) = toCNF (Dsj [Neg p, q])
toCNF (Equiv p q) = toCNF (Dsj [Cnj [p, q], Cnj [Neg p, Neg q]])

toCNF (Cnj [p, q]) = Cnj [toCNF p, toCNF q]
toCNF (Dsj [p, q]) = Dsj [toCNF p, toCNF q]

toCNF (Neg (Cnj [p, q])) = Dsj [toCNF (Neg p), toCNF (Neg q)]
toCNF (Neg (Dsj [p, q])) = Cnj [toCNF (Neg p), toCNF (Neg q)]
toCNF (Neg (Neg p)) = toCNF p
toCNF (Neg (Prop p)) = Neg (Prop p)

toCNF (Prop p) = Prop p

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
        subformula2 = formula' (div n 2)

-- quickCheck $ forAll formula (\ f -> isCNF (toCNF f))

-- +(+(3 0) (2==>-1))

-- formulaCNF =

-- data RandomForm = RandomForm Form deriving Show
--
-- instance Arbitrary RandomForm where
--     arbitrary = fmap RandomForm (listOf $ elements (map chr [0..127]))
