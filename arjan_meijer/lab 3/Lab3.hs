module Lab3 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture3

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


main = do
    print $ contradiction $ Cnj [Prop 1, Neg (Prop 1)]
    print $ tautology $ Dsj [Prop 1, Neg (Prop 1)]
    print $ entails p (Cnj [p, q])