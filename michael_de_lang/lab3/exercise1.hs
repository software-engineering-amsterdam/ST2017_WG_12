import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

allPropsExistIn :: Form -> Form -> Bool
allPropsExistIn x y = all (\x' -> elem x' (propNames y)) (propNames x)

-- | logical entailment 
entails :: Form -> Form -> Bool
entails x y = allPropsExistIn x y && all (\v -> evl v y) relevantCases
    where relevantCases = filter (\v -> evl v x) (allVals x)

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv x y = allPropsExistIn x y && all (\v -> evl v x == evl v y) (allVals x)

main = do
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


-- time taken: 45m

-- These tests can be automated, but for the sake of brevity/clarity, I only tested a couple of known correct/incorrect values.
-- Not to mention that automating these would require the generator made in exercise 4.

-- output: 
-- oipo@sd-59673:~/uva/software_testing/STS1/michael_de_lang/lab3$ ghc -O2 exercise1.hs && ./exercise1
-- [2 of 2] Compiling Main             ( exercise1.hs, exercise1.o )
-- Linking exercise1 ...
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

-- Conclusion: The functions work correctly for the given forms.

