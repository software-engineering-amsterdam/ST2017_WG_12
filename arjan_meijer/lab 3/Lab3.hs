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

entails :: Form -> Form -> Bool
entails a b = and ( map (\v -> evl (v ++ (fillVals b a)) b) (filter (\v -> evl v a) (allVals a)))
equiv p q = and (map (\v -> (evl v p) == (evl (v ++ fillVals q p) q)) (allVals p))

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

invalidInput :: String
invalidInput = "(x)"

validInput :: String
validInput = "+(1 2)"

testParseInput :: [Integer] -> Bool
testParseInput (x:xs) | x == 0 = parse (validInput) /= [] && testParseInput xs
                 | otherwise = parse (invalidInput) == [] && testParseInput xs
testParseInput [] = True

reflexiveParse :: String -> Bool
reflexiveParse s = show (parse (show (parse s))) == s

testParse :: Bool
testParse = True
   

-- Exercise 3
-- Time 300 Minutes

toCNF :: Form -> Form
toCNF (Impl f1 f2) = toCNF (Dsj [(Neg f1),f2])
toCNF (Equiv f1 f2) = toCNF (Cnj [Impl f1 f2, Impl f2 f1])
toCNF (Neg (Dsj fs)) = toCNF (Cnj (map (\x -> toCNF(Neg x)) fs))
toCNF (Neg (Cnj fs)) = toCNF (Dsj (map (\x -> toCNF(Neg x)) fs))
toCNF (Neg (Neg f)) = toCNF f
toCNF (Neg (Prop x)) = Neg (Prop x)
toCNF (Neg f) = toCNF(Neg (toCNF f))
toCNF (Cnj fs) | fFalse (head fs) == (Cnj fs) = Cnj fs
               | otherwise = noSingle (Cnj (map (\x -> toCNF x) (validCnj fs)))
toCNF (Dsj fs) | fTrue (head fs) == (Dsj fs) = Dsj fs
               | otherwise = noSingle (Dsj(parseDsj ((map (\x -> toCNF x) (parseDsj fs)))))
toCNF f | contradiction f = fFalse f
        | tautology f = fTrue f
        | otherwise = f

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
cnfTest f = (and (map (\x -> evl x f == evl x y) (allVals f))) && not (isInfixOf "==>" (show y)) && not (isInfixOf "<=>" (show y)) where y = toCNF f

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