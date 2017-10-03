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
                               where subform = formNProps maxProp (n - 1)

formNCnjProps :: Int -> Int -> Gen Form
formNCnjProps maxProp 0 = liftM Prop (choose (1, maxProp))
formNCnjProps maxProp n | n > 0 = oneof [liftM Prop (choose (1, maxProp)), liftM Cnj (vectorOf 2 subform)]
                               where subform = formNCnjProps maxProp (n - 1)

-- Convenience generator for formNGen 1000
formGen :: Gen Form
formGen = formNGen 1000 5

-- Generate forms with propositions between [1..n]
formNGen :: Int -> Int -> Gen Form
formNGen n x = resize x $ sized (formNProps n)

-- Generate forms that are always in CNF with propositions between [1..n] and forms up to a max of N deep 
formNCnjGen :: Int -> Int -> Gen Form
formNCnjGen n x = resize x $ sized (formNCnjProps n)

-- generate N forms with the given generator
genN :: Int -> Gen Form -> IO [Form]
genN n gen = sequence $ take n $ repeat $ generate gen

-- } End of generators

-- Exercise 1
-- Time spent in minutes:
-- Michael      45
-- Constantijn  60
-- Niels        60
-- Arjan        100

-- We used the exercise of Constantijn because he had the most efficient code
-- We created a new equiv function as a group as well as modified entails

allPropsExistIn :: Form -> Form -> Bool
allPropsExistIn p q = all (\p' -> elem p' (propNames q)) (propNames p)

contradiction, tautology :: Form -> Bool
contradiction = not . satisfiable
tautology f = all (\ v -> evl v f) (allVals f)

entails, equiv :: Form -> Form -> Bool
entails p q = allPropsExistIn p q && and (map (\ v -> evl v q) (filter (\ v -> evl v p) (allVals p)))

equiv p q = tautology (Equiv p q)
-- Source: http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
-- equiv is a tautology of an equivalance

exerciseOne = do
    print "Exercise one"
    print $ contradiction $ Cnj [Prop 1, Neg (Prop 1)] -- true
    print $ contradiction $ Prop 1 -- false
    print $ tautology $ Dsj [Prop 1, Neg (Prop 1)] -- true
    print $ tautology $ Prop 1 -- false
    print $ entails (Cnj [Prop 1,Prop 2]) (Dsj [Prop 1, Prop 2]) -- true
    print $ entails (Prop 1) (Prop 2) -- false
    print $ entails (Impl (Prop 1) (Prop 2)) (Impl (Prop 2) (Prop 1)) -- false
    print $ equiv (Neg (Cnj [Prop 1, Prop 2])) (Dsj [Neg (Prop 1), Neg (Prop 2)]) -- true
    print $ equiv (Prop 1) (Prop 2) -- false
    print $ equiv (Cnj [Prop 1, Prop 2]) (Dsj [Prop 1, Prop 2]) -- false

-- output:
-- "Exercise one"
-- True
-- False
-- True
-- False
-- True
-- False
-- False
-- True
-- False
-- False


-- Exercise 2
-- Time spent in minutes:
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
    print "Exercise two"
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
-- "Exercise two"
-- True
-- True
-- True
-- "Success"
-- "Executing quickCheck, may take a while"
-- +++ OK, passed 100 tests.

-- conclusion: The parse function works correctly, given the restraint that proposition names are exclusively positive numbers. Otherwise, either change the representation and parsing of the minus sign,
-- or change the type for the names of propositions.

-- Exercise 3
-- Time spent in minutes:
-- Michael      480
-- Constantijn  130
-- Niels        100
-- Arjan        300

-- We found out our implementation of toCNF wasn't complete, so Arjan and Michael made two separate implementations.
-- Michael's implementation is presented here, since it is the most complete. If we had more time, we would have chosen Arjan's implementation since his is made to work on conjunctions
-- and disjunctions with arbitrary length, rather than Michael's implementation which assumes that it is exactly two.
-- automated test

-- Mathematically multiply x with y in various cases. 
-- e.g. 1 v (2 ^3) becomes (1 v 2) ^ (1 v 3)
combineF :: Form -> Form -> Form
combineF (Prop x) (Cnj fs) = Cnj (map (\f -> Dsj [Prop x, f]) fs)
combineF (Cnj fs) (Prop x) = Cnj (map (\f -> Dsj [Prop x, f]) fs)
combineF (Prop x) (Dsj fs) = Dsj (map (\f -> Dsj [Prop x, f]) fs)
combineF (Dsj fs) (Prop x) = Dsj (map (\f -> Dsj [Prop x, f]) fs)
combineF (Cnj fs) (Cnj xs) = Cnj (concat (map (\f -> (map (\x -> Dsj [f, x]) xs)) fs))
combineF (Cnj fs) (Dsj x) = Cnj (map (\f -> Dsj [Dsj x ,f]) fs)
combineF (Dsj x) (Cnj fs) = Cnj (map (\f -> Dsj [Dsj x, f]) fs)
combineF x y = Dsj [x, y]

-- Find cases where a disjunction contains one or two conjunctions.
distributeORs :: Form -> Form
distributeORs (Prop x) = Prop x
distributeORs (Neg x) = Neg (distributeORs x)
distributeORs (Dsj [(Cnj p), (Cnj q)]) = distributeORs $ combineF (Cnj p) (Cnj q)
distributeORs (Dsj [p, (Cnj q)]) = distributeORs $ combineF p (Cnj q)
distributeORs (Dsj [(Cnj p), q]) = distributeORs $ combineF (Cnj p) q
distributeORs (Cnj xs) = Cnj (map distributeORs xs)
distributeORs (Dsj xs) = Dsj (map distributeORs xs)

-- This function recurses into itself until the outcome doesn't change anymore.
-- This is because distributeORs doesn't deal with two-level deep possibilities, that would be an insane
-- Acceptable input is only valid while the length of elements in a Dsj or Cnj is exactly 2.
toCNF:: Form -> Form
toCNF f | outcome /= f = toCNF outcome
      | otherwise = outcome
      where outcome = (distributeORs . nnf . arrowfree) f

exerciseThree = do
    print "Exercise three"
    print $ toCNF $ head $ parse "+(1 *(2 +(3 *(4 +(5 (6==>7))))))"
-- output:
-- "Exercise three"
-- *(+(1 2) *(+(1 +(3 4)) +(1 +(3 +(5 +(-6 7))))))


-- Exercise 4
-- Time spent in minutes:
-- Michael      480
-- Constantijn  50
-- Niels        270
-- Arjan        300

-- We used the implementations of exercise 4 of Michael and Niels because we couldn't decide which is better.
-- The quickcheck implementation integrates well in quickcheck, but has the tendency to generate very balanced forms (implies and forms with equal length subforms)
-- The std random version is nice because it doesn't have this overly balanced tree generation, but it isn't easily integrated with quickcheck.

-- Niels had a non-quickCheck implementation and Michael had a quickCheck implementation

-- During exercise 4, it was discovered that the toCNF function in exercise 3 is faulty.
-- Some constructs generated by the form generator end up creating infinite loops.
-- Due to time constraints, this issue has not been resolved.

-- QuickCheck implementation
-- takes nForms from a filtered list of IO forms.
filterOnNProps :: Int -> Int -> IO [Form] -> IO [Form]
filterOnNProps nForms nProps forms = do
    forms' <- forms
    return $ take nForms $ filter (\x -> length (propNames x) == nProps) forms'

-- Generate exactly nForms amount of Forms, all with exactly nProps.
-- I would've liked to use the repeat function here, but the sequence function in filterOnNProps evaluates the entire list.
genWithExactlyNProps :: Int -> Int -> IO [Form]
genWithExactlyNProps nForms nProps = filterOnNProps nForms nProps $ genN (nForms*50) (formNGen nProps (nProps+2))

-- properties to test the generators themselves

-- The formGen generator only rarily generates non-satisfiable forms.
prop_generatesMostlySatisfiableForms ::  Property
prop_generatesMostlySatisfiableForms = monadicIO $ do
    s <- run $ genN 50 formGen
    assert $ length (filter satisfiable s) > 48

prop_neverMoreThanNProps ::  Property
prop_neverMoreThanNProps = monadicIO $ do
    s <- run $ genN 5000 (formNGen 5 5)
    assert $ all (\x -> length (propNames x) <= 5) s

prop_exactlyNProps :: Property
prop_exactlyNProps = monadicIO $ do
    s <- run $ genWithExactlyNProps 5000 5
    assert $ length s == 5000 && all (\x -> length (propNames x) == 5) s

-- properties to test exercise 3

prop_CNFInputUnaltered :: Property
prop_CNFInputUnaltered = monadicIO $ do
    s <- run $ generate $ formNCnjGen 5 8
    assert $ toCNF s == s

-- prop_CNF fails because the CNF function is not good enough. It generates things like "-*((1==>2) (1<=>4)))" which end up causing infinite recursion.
prop_CNF :: Property
prop_CNF = monadicIO $ do
    s <- run $ genN 50 (formNGen 4 5)
    assert $ length (filter (\x -> toCNF x /= x) s) > 48


exerciseFourQuickCheck = do
    print "exerciseFourQuickCheck running, this may take a while - four tests"
    quickCheck prop_generatesMostlySatisfiableForms
    quickCheck prop_neverMoreThanNProps
    quickCheck prop_exactlyNProps
    quickCheck prop_CNFInputUnaltered
    -- quickCheck $ within 5000000 prop_CNF -- this times out in ghci but doesn't when compiling.

-- output: see below



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

prop_affected, prop_unaffected, prop_equivalance :: Form -> Bool

-- A non cnf should not change
prop_affected f = toCNF f /= f

-- A cnf should not change
prop_unaffected f = toCNF f == f

-- The cnf should yield the same truth table
prop_equivalance f = equiv (toCNF f) (f)

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

-- output:
-- "exerciseFourQuickCheck running, this may take a while - four tests"
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- "*(0 +(1 *(3 *(0 +(1 +(3 +(3 *(1 *(1 +(*(3 +(+(2 *(0 +(1 *(1 +(2 +(*(*(+(3 +(2 +(3 +(2 +(*(*(+(+(1 *(+(0 *(1 *(+(+(+(1 *(+(3 *(2 +(2 +(+(+(1 *(2 *(0 +(+(*(3 +(*(2 *(0 +(2 +(+(3 *(*(3 *(2 *(1 +(*(+(+(+(0 +(+(+(0 +(0 *(0 +(1 +(*(*(1 +(1 *(1 *(+(3 +(1 *(+(+(0 +(2 +(+(3 *(*(0 *(0 +(+(3 *(2 *(+(+(2 +(*(0 +(*(+(0 +(+(0 *(3 0)) 0)) 1) 3)) 2)) 0) 2))) 2))) 0)) 0))) 0) 2))) 1)))) 3) 1))))) 0) 3)) 1) 3) 1) 3)))) 0)) 2)))) 0)) 2) 2)))) 3) 1)))) 0)) 1) 2) 2))) 1)) 0) 0) 0) 0))))) 1) 3) 2)))))) 1)) 1))))))))))"
-- "PARSED  [*(0 +(1 *(3 *(0 +(1 +(3 +(3 *(1 *(1 +(*(3 +(+(2 *(0 +(1 *(1 +(2 +(*(*(+(3 +(2 +(3 +(2 +(*(*(+(+(1 *(+(0 *(1 *(+(+(+(1 *(+(3 *(2 +(2 +(+(+(1 *(2 *(0 +(+(*(3 +(*(2 *(0 +(2 +(+(3 *(*(3 *(2 *(1 +(*(+(+(+(0 +(+(+(0 +(0 *(0 +(1 +(*(*(1 +(1 *(1 *(+(3 +(1 *(+(+(0 +(2 +(+(3 *(*(0 *(0 +(+(3 *(2 *(+(+(2 +(*(0 +(*(+(0 +(+(0 *(3 0)) 0)) 1) 3)) 2)) 0) 2))) 2))) 0)) 0))) 0) 2))) 1)))) 3) 1))))) 0) 3)) 1) 3) 1) 3)))) 0)) 2)))) 0)) 2) 2)))) 3) 1)))) 0)) 1) 2) 2))) 1)) 0) 0) 0) 0))))) 1) 3) 2)))))) 1)) 1))))))))))]"
-- "*(2 *(3 +(2 *(1 0))))"


-- because doing these in ghci is too slow, we added a main which can be compiled with optimizations.
main = do
    exerciseOne
    exerciseTwo
    exerciseThree
    exerciseFourQuickCheck
    exerciseFour
    exerciseThreeTest
