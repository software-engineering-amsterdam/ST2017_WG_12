module Lab3 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture3
import Debug.Trace
import Control.Monad
import Control.Exception

-- Exercise 1
-- Time 100 Minutes

contradiction, tautology :: Form -> Bool
contradiction f = and (map (\ v -> not (evl v f)) (allVals f))
tautology f = and (map (\v -> evl v f) (allVals f))

entails, equiv :: Form -> Form -> Bool
entails a b = and ( map (\v -> evl (v ++ (fillVals b a)) b) (filter (\v -> evl v a) (allVals a)))
equiv p q = tautology (Equiv p q)
-- Source: http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
-- equiv is a tautology of an equivalance

fillVals :: Form -> Form -> Valuation
fillVals a b = map (\x -> (x,False)) (filter (\x -> not (elem x (propNames b))) (propNames a))

exerciseOne = do
    print $ contradiction $ Cnj [Prop 1, Neg (Prop 1)]
    print $ tautology $ Dsj [Prop 1, Neg (Prop 1)]
    print $ entails p (Cnj [p, q])

-- Exercise 2
-- Time XX Minutes - 13:00

-- Parse method preconditions:
--   The formula should be valid

-- Parse methods postconditions:
--   Any result of a parsed formula must be valid
--   For the parse method itself (Reflexive)

-- Parse method behaviour
--  The parse method should return an empty array is the
--  input was invalid

ioAnd :: IO Bool -> IO Bool -> IO Bool
ioAnd a b = do
              x <- a
              y <- b
              return (x && y)

validInput = do
     let n = 50
     opp <- randomSequenceN n 1 5
     vars <- randomSequenceN n 0 10
     coins <- randomSequenceN n 0 1
     return (formulaGenerator (toTuples opp vars coins) "0")

validParse :: IO String -> IO Bool
validParse input = do 
              x <- input
              return ((show (parse x)) == x)


invalidInput :: IO String
invalidInput = return "(x)"

testParseInput :: [Integer] -> IO Bool
testParseInput (x:xs) | x == 0 = ioAnd (validParse validInput) (testParseInput xs)
                      | otherwise = ioAnd (validParse invalidInput) (testParseInput xs)
testParseInput [] = do return True

reflexiveParse :: String -> Bool
reflexiveParse s = show (parse (show (parse s))) == s

-- CREDITS: NIELS
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

-- Exercise 3
-- Time 300 Minutes

tCNF :: Form -> Form
tCNF (Impl f1 f2) = tCNF (Dsj [Neg f1, f2])
tCNF (Equiv f1 f2) = tCNF (Cnj [Impl f1 f2, Impl f2 f1])
tCNF (Dsj fs) = Dsj (map (\x -> tCNF x) fs)
tCNF (Cnj fs) = Cnj (map (\x -> tCNF x) fs)
tCNF (Neg (Dsj fs)) = tCNF (Cnj (map (\x -> tCNF(Neg x)) fs))
tCNF (Neg (Cnj fs)) = tCNF (Dsj (map (\x -> tCNF(Neg x)) fs))
tCNF (Neg (Neg f)) = tCNF f
tCNF (Neg (Prop x)) = Neg (Prop x)
tCNF (Neg f) = tCNF(Neg (tCNF f))
tCNF f = f

flatten :: Form -> Form
flatten (Neg (Prop x)) = Neg (Prop x)
flatten (Neg f) = flatten(Neg (flatten f))
flatten (Cnj fs) | fFalse (head fs) == (Cnj fs) = Cnj fs
               | otherwise = noSingle (Cnj (map (\x -> flatten x) (validCnj fs)))
flatten (Dsj fs) | fTrue (head fs) == (Dsj fs) = Dsj fs
               | otherwise = noSingle (Dsj(parseDsj ((map (\x -> flatten x) (parseDsj fs)))))
flatten f = f

parseDsj :: [Form] -> [Form]
parseDsj fs = concat (map (\x -> pdsj x) fs)
              where 
               pdsj (Dsj fs) =fs
               pdsj f = [f]

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

cnfTest :: Form -> Bool
cnfTest f = (and (map (\x -> evl x f == evl x y) (allVals f))) && not (isInfixOf "==>" (show y)) && not (isInfixOf "<=>" (show y)) where y = tCNF f

-- CREDITS: MICHEAL
-- inspiration http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#16
form' :: Int -> Gen Form
form' 0 = liftM Prop arbitrary
form' n | n > 0 = oneof [liftM Prop arbitrary, liftM Neg subform,
                        liftM Cnj (vectorOf 2 subform), liftM Dsj (vectorOf 2 subform),
                        liftM2 Impl subform subform, liftM2 Equiv subform subform]
            where subform = form' (div n 2)


testGen = resize 10 $ sized form'


--main = quickCheck $ forAll testGen (\x -> satisfiable x)
exerciseThree = do
                  quickCheck $ forAll testGen (\x -> cnfTest x)