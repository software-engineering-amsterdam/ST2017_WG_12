import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Lecture3
import Control.Monad

-- inspiration http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#16
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

-- takes nForms from a filtered list of IO forms.
filterOnNProps :: Int -> Int -> IO [Form] -> IO [Form]
filterOnNProps nForms nProps forms = do
    forms' <- forms
    return $ take nForms $ filter (\x -> length (propNames x) == nProps) forms'

-- Generate exactly nForms amount of Forms, all with exactly nProps.
-- I would've liked to use the repeat function here, but the sequence function in filterOnNProps evaluates the entire list.
genWithExactlyNProps :: Int -> Int -> IO [Form]
genWithExactlyNProps nForms nProps = filterOnNProps nForms nProps $ genN (nForms*50) (formNGen nProps)

-- properties to test the generators themselves

-- The formGen generator only rarily generates non-satisfiable forms.
prop_generatesMostlySatisfiableForms ::  Property
prop_generatesMostlySatisfiableForms = monadicIO $ do
    s <- run $ genN 50 formGen
    assert $ length (filter satisfiable s) > 48

prop_neverMoreThanNProps ::  Property
prop_neverMoreThanNProps = monadicIO $ do
    s <- run $ genN 5000 (formNGen 5)
    assert $ all (\x -> length (propNames x) <= 5) s

prop_exactlyNProps :: Property
prop_exactlyNProps = monadicIO $ do
    s <- run $ genWithExactlyNProps 5000 5
    assert $ length s == 5000 && all (\x -> length (propNames x) == 5) s

-- properties to test exercise 3

main = do
    quickCheck prop_generatesMostlySatisfiableForms
    quickCheck prop_neverMoreThanNProps
    quickCheck prop_exactlyNProps


-- time taken: 6 hours (mainly figuring out how to generate forms at all and then some more time to figure out how to generate forms with a specific amount of propositions)
-- 
