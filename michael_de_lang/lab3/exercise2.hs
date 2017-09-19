import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Lecture3
import Control.Monad

-- inspiration: see exercise 4
formNProps :: Int -> Int -> Gen Form
formNProps maxProp 0 = liftM Prop (choose (1, maxProp))
formNProps maxProp n | n > 0 = oneof [liftM Prop (choose (1, maxProp)), liftM Neg subform,
                                      liftM Cnj (vectorOf 2 subform), liftM Dsj (vectorOf 2 subform),
                                      liftM2 Impl subform subform, liftM2 Equiv subform subform]
                               where subform = formNProps maxProp (div n 2)

-- Convenience generator for formNGen 1000
formGen :: Gen Form
formGen = formNGen 1000

-- Generate forms with propositions between [1..n]
formNGen :: Int -> Gen Form
formNGen n = resize 30 $ sized (formNProps n)

-- generate N forms with the given generator
genN :: Int -> Gen Form -> IO [Form]
genN n gen = sequence $ take n $ repeat $ generate gen

filterSatisfiable :: IO [Form] -> IO [Form]
filterSatisfiable forms = do
    forms' <- forms
    return $ filter (\x -> satisfiable x) forms'

prop_parsingFormShouldGiveOriginal :: Property
prop_parsingFormShouldGiveOriginal = monadicIO $ do
    s <- run $ filterSatisfiable $ genN 500 formGen
    assert $ all (\x -> (parse (show x) !! 0) == x) s

main = do
    print $ (parse "*(2 3)") == ([Cnj [Prop 2, Prop 3]])
    print $ (parse "-*(2 3)") == ([Neg (Cnj [Prop 2, Prop 3])])
    print $ (parse "+(2 *(3 -4))") == ([Dsj [Prop 2, Cnj [Prop 3, Neg (Prop 4)]]])
    let testForm = Dsj [Prop 2, Cnj [Prop 3, Neg (Prop 4)]]
    if parse (show testForm) !! 0 == testForm then
        print "success"
    else
        print "fail"
    quickCheck prop_parsingFormShouldGiveOriginal

-- time taken: 60m

-- My test approach is to first do a few quick manual sanity tests, another test to see if my general idea is correctly (parsing the text representation of a Form should give back the original Form) and then automate this with QuickCheck.
-- During my testing, I found that the parser doesn't handle negative ints well, parsing them instead as negations. A suggested fix would be to represent negations as a different character than the minus sign.
-- To workaround this, I simply altered the generator to only generate propositions with positive 'names'.

-- output: 
-- oipo@oipo-B85M-HD3 ~/Documents/UvA/master/Software Testing/ST2017_WG_12/michael_de_lang/lab3 $ ghc -O2 exercise2.hs && ./exercise2
-- [2 of 2] Compiling Main             ( exercise2.hs, exercise2.o )
-- Linking exercise2 ...
-- True
-- True
-- True
-- "success"
-- +++ OK, passed 100 tests.

-- conclusion: The parse function works correctly, given the restraint that proposition names are exclusively positive numbers. Otherwise, either change the representation and parsing of the minus sign,
-- or change the type for the names of propositions.

