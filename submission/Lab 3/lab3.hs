module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Test.QuickCheck.Monadic
import Control.Monad
import Debug.Trace

-- Generators for exercise 2 and 4 - By Michael {
formNProps :: Int -> Int -> Gen Form
formNProps maxProp 0 = liftM Prop (choose (1, maxProp))
formNProps maxProp n | n > 0 = oneof [liftM Prop (choose (1, maxProp)), liftM Neg subform,
                                      liftM Cnj (vectorOf 2 subform), liftM Dsj (vectorOf 2 subform),
                                      liftM2 Impl subform subform, liftM2 Equiv subform subform]
                               where subform = formNProps maxProp (div n 2)

-- Convenience generator for formNGen 1000
formGen :: Gen Form
formGen = formNGen 1000 30

-- Generate forms with propositions between [1..n]
formNGen :: Int -> Int -> Gen Form
formNGen n x = resize x $ sized (formNProps n)

-- generate N forms with the given generator
genN :: Int -> Gen Form -> IO [Form]
genN n gen = sequence $ take n $ repeat $ generate gen

-- } End of generators

-- Exercise 1
-- Time spent: 
-- Michael      45
-- Constantijn  60
-- Niels        60
-- Arjan        100

-- We used the exercise of Constantijn because he had the most efficient code
-- We created a new equiv function as a group

contradiction, tautology :: Form -> Bool
contradiction = not . satisfiable
tautology f = all (\ v -> evl v f) (allVals f)

entails, equiv :: Form -> Form -> Bool
entails p q = and (map (\ v -> evl v q) (filter (\ v -> evl v p) (allVals p)))

equiv p q = tautology (Equiv p q)
-- Source: http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
-- equiv is a tautology of an equivalance

exerciseOne = do
    print $ contradiction $ Cnj [Prop 1, Neg (Prop 1)]
    print $ tautology $ Dsj [Prop 1, Neg (Prop 1)]
    print $ entails p (Cnj [p, q])
    print $ equiv (Impl p q) (Dsj [Neg p, q])

-- Exercise 2
-- Time spent: 
-- Michael      60
-- Constantijn  5 - Not completed
-- Niels        30 - Not completed
-- Arjan        120 - Not completed

-- We used the exercise of Michael because he has the (most) complete exercise with the best tests
-- We made some small changes as a team

filterSatisfiable :: IO [Form] -> IO [Form]
filterSatisfiable forms = do
    forms' <- forms
    return $ filter satisfiable forms'

prop_parsingFormShouldGiveOriginal :: Property
prop_parsingFormShouldGiveOriginal = monadicIO $ do
    s <- run $ filterSatisfiable $ genN 500 formGen
    assert $ all (\x -> (parse (show x) !! 0) == x) s

exerciseTwo = do
    print $ (parse "*(2 3)") == ([Cnj [Prop 2, Prop 3]])
    print $ (parse "-*(2 3)") == ([Neg (Cnj [Prop 2, Prop 3])])
    print $ (parse "+(2 *(3 -4))") == ([Dsj [Prop 2, Cnj [Prop 3, Neg (Prop 4)]]])
    let testForm = Dsj [Prop 2, Cnj [Prop 3, Neg (Prop 4)]]
    if parse (show testForm) !! 0 == testForm then
        print "Success"
    else
        print "Fail"
    print "Executing quickCheck, may take a while"
    quickCheck prop_parsingFormShouldGiveOriginal


-- Our test approach is to first do a few quick manual sanity tests, another test to see if our general idea is correctly (parsing the text representation of a Form should give back the original Form) and then automate this with QuickCheck.
-- During our testing, We found that the parser doesn't handle negative ints well, parsing them instead as negations. A suggested fix would be to represent negations as a different character than the minus sign.
-- To workaround this, We simply altered the generator to only generate propositions with positive 'names'.

-- output: 
-- True
-- True
-- True
-- "Success"
-- "Executing quickCheck, may take a while"
-- +++ OK, passed 100 tests.

-- conclusion: The parse function works correctly, given the restraint that proposition names are exclusively positive numbers. Otherwise, either change the representation and parsing of the minus sign,
-- or change the type for the names of propositions.

-- Exercise 3
-- Time spent: 
-- Michael      120 - Not completed
-- Constantijn  130
-- Niels        100
-- Arjan        300

-- We used the Exercise of Constantijn because he had the most compact ToCNF method
-- We used the test of Arjan who used the generator of Michael because this was the best
-- automated test

toCNF :: Form -> Form
toCNF = nnf . arrowfree

fFalse, fTrue :: Form -> Form
fFalse f = Cnj [x, Neg x] where x = Prop (head (propNames f))
fTrue f = Dsj [x, Neg x] where x = Prop (head (propNames f))

noSingle :: Form -> Form
noSingle (Cnj fs) | length fs == 1 = head fs
                  | otherwise = Cnj fs
noSingle (Dsj fs) | length fs == 1 = head fs
                  | otherwise = Dsj fs

validCnj :: [Form] -> [Form]
validCnj fs | contradiction (Cnj fs) = [fFalse (head fs)]
            | otherwise = fs

parseDsj :: [Form] -> [Form]
parseDsj fs = concat (map (\x -> pdsj x) fs)
              where 
               pdsj (Dsj fs) =fs
               pdsj f = [f]

flatten :: Form -> Form
flatten (Neg (Prop x)) = Neg (Prop x)
flatten (Neg f) = flatten(Neg (flatten f))
flatten (Cnj fs) | fFalse (head fs) == (Cnj fs) = Cnj fs
               | otherwise = noSingle (Cnj (map (\x -> flatten x) (validCnj fs)))
flatten (Dsj fs) | fTrue (head fs) == (Dsj fs) = Dsj fs
               | otherwise = noSingle (Dsj(parseDsj ((map (\x -> flatten x) (parseDsj fs)))))
flatten f = f

cnfTest :: Form -> Bool
cnfTest f = and (map (\x -> evl x f == evl x y) (allVals f)) && not (isInfixOf "==>" (Prelude.show (y :: Form))) && not (isInfixOf "<=>" (Prelude.show (y :: Form))) where y = flatten ( toCNF f)

-- CREDITS: MICHEAL
-- inspiration http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#16

exerciseThree = do
                  quickCheck $ forAll (formNGen 1000 10) (\x -> cnfTest x)

-- Exercise 4
-- Time spent: 
-- Michael      360
-- Constantijn  50
-- Niels        270
-- Arjan        30 - Combined with exercise 3
