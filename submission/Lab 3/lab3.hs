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

-- We used the implementations of exercise 4 of Michael and Niels
-- Niels had a non-quickCheck implementation and Michael had a quickCheck implementation

-- QuickCheck implementation
-- takes nForms from a filtered list of IO forms.
filterOnNProps :: Int -> Int -> IO [Form] -> IO [Form]
filterOnNProps nForms nProps forms = do
    forms' <- forms
    return $ take nForms $ filter (\x -> length (propNames x) == nProps) forms'

-- Generate exactly nForms amount of Forms, all with exactly nProps.
-- I would've liked to use the repeat function here, but the sequence function in filterOnNProps evaluates the entire list.
genWithExactlyNProps :: Int -> Int -> IO [Form]
genWithExactlyNProps nForms nProps = filterOnNProps nForms nProps $ genN (nForms*50) (formNGen nProps 30)

-- properties to test the generators themselves

-- The formGen generator only rarily generates non-satisfiable forms.
prop_generatesMostlySatisfiableForms ::  Property
prop_generatesMostlySatisfiableForms = monadicIO $ do
    s <- run $ genN 50 formGen
    assert $ length (filter satisfiable s) > 48

prop_neverMoreThanNProps ::  Property
prop_neverMoreThanNProps = monadicIO $ do
    s <- run $ genN 5000 (formNGen 5 30)
    assert $ all (\x -> length (propNames x) <= 5) s

prop_exactlyNProps :: Property
prop_exactlyNProps = monadicIO $ do
    s <- run $ genWithExactlyNProps 5000 5
    assert $ length s == 5000 && all (\x -> length (propNames x) == 5) s

-- properties to test exercise 3

exerciseFourQuickCheck = do
    print "QuickCheck running, this may take a while - three tests"
    quickCheck prop_generatesMostlySatisfiableForms
    quickCheck prop_neverMoreThanNProps
    quickCheck prop_exactlyNProps


-- Non-quickcheck implementation
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

-- A cnf form should not change
prop_unAffected f = toCNF f == f

-- A non cnf from should not change
prop_Affected f = toCNF f /= f

exerciseFour = do let n = 100
                  opp <- randomSequenceN n 1 2
                  vars <- randomSequenceN n 0 3
                  coins <- randomSequenceN n 0 1
                  print $ formulaGenerator (toTuples opp vars coins) "0"
                  print ( "PARSED  " ++ show (parse (formulaGenerator (toTuples opp vars coins) "0")))

-- Exercise 3 tests with the implementation of 4
exerciseThreeTest = do let n = 4
                       opp <- randomSequenceN n 1 2
                       vars <- randomSequenceN n 0 3
                       coins <- randomSequenceN n 0 1
                       let form = formulaGenerator (toTuples opp vars coins) "0"
                       print form
                       print "4"