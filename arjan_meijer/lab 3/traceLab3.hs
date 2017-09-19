module Lab3 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture3
import Debug.Trace
import Control.Monad

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
-- Time XX Minutes


-- Exercise 3
-- Time 15:35

toCNF :: Form -> Form
toCNF (Impl f1 f2) = trace("IMPL " ++ show f1 ++ show f2) toCNF (Dsj [(Neg f1),f2])
toCNF (Equiv f1 f2) = trace("EQUIV " ++ show f1 ++ show f2) toCNF (Cnj [Impl f1 f2, Impl f2 f1])
toCNF (Neg (Dsj fs)) = trace("Neg Dsj " ++ show fs) toCNF (Cnj (map (\x -> toCNF(Neg x)) fs))
toCNF (Neg (Cnj fs)) = trace("Neg Cnj " ++ show fs) toCNF (Dsj (map (\x -> toCNF(Neg x)) fs))
toCNF (Neg (Neg f)) = trace("Neg Neg " ++ show f) toCNF f
toCNF (Neg (Prop x)) = trace("Neg Prop " ++ show x)Neg (Prop x)
toCNF (Neg f) = trace ("Neg " ++ show f) toCNF(Neg (toCNF f))
toCNF (Cnj fs) | fFalse (head fs) == (Cnj fs) = Cnj fs
               | otherwise = trace("Cnj " ++ show fs) noSingle (Cnj (map (\x -> toCNF x) (validCnj fs)))
toCNF (Dsj fs) | fTrue (head fs) == (Dsj fs) = Dsj fs
               | otherwise = trace("Dsj " ++ show fs) noSingle (Dsj(parseDsj ((map (\x -> toCNF x) (parseDsj fs)))))
toCNF f | contradiction f = trace("Con " ++ show f) fFalse f
        | tautology f = trace("Taut " ++ show f) fTrue f
        | otherwise = trace("Prop " ++ show f) f

parseDsj :: [Form] -> [Form]
parseDsj fs = trace("PARSEDSJ " ++ show fs)concat (map (\x -> pdsj x) fs)
              where 
               pdsj (Dsj fs) = trace("FOUND DSJ" ++ show fs)fs
               pdsj f = trace("OTHER IN PARSE " ++ show f)[toCNF f]

fFalse, fTrue :: Form -> Form
fFalse f = Cnj [x, Neg x] where x = Prop (head (propNames f))
fTrue f = Dsj [x, Neg x] where x = Prop (head (propNames f))

noSingle :: Form -> Form
noSingle (Cnj fs) | length fs == 1 = trace("SINGLE VAL " ++ show fs) head fs
                  | otherwise = trace("NONSINGLE") Cnj fs
noSingle (Dsj fs) | length fs == 1 = trace("SINGLE VAL " ++ show fs)head fs
                  | otherwise = trace("NONSINGLE") Dsj fs

validCnj :: [Form] -> [Form]
validCnj fs | contradiction (Cnj fs) = [fFalse (head fs)]
            | otherwise = fs


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

exerciseFourTests = do s <- randomSequence
                       print $ show $ toCNF (head (parse (formulaGenerator s "0")))

cnfTest :: Form -> Bool
cnfTest f = and (map (\x -> trace(show (evl x f == evl x y) ++ show x) evl x f == evl x y) (allVals f) )where y = toCNF f

-- CREDITS MICHEAL
-- inspiration http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#16
form' :: Int -> Gen Form
form' 0 = liftM Prop arbitrary
form' n | n > 0 = oneof [liftM Prop arbitrary, liftM Neg subform,
                        liftM Cnj (vectorOf 2 subform), liftM Dsj (vectorOf 2 subform),
                        liftM2 Impl subform subform, liftM2 Equiv subform subform]
            where subform = form' (div n 2)


testGen = sized form'


--main = quickCheck $ forAll testGen (\x -> satisfiable x)
exerciseThree = do
                  quickCheck $ forAll testGen (\x -> cnfTest x)