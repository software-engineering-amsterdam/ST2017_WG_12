import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Lecture3
import Control.Monad
import Debug.Trace 

-- inspiration http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#16
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

-- Generate forms with propositions between [1..n] and forms up to a max of N deep 
formNGen :: Int -> Int -> Gen Form
formNGen n x = resize x $ sized (formNProps n)

-- Generate forms that are always in CNF with propositions between [1..n] and forms up to a max of N deep 
formNCnjGen :: Int -> Int -> Gen Form
formNCnjGen n x = resize x $ sized (formNCnjProps n)

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
genWithExactlyNProps nForms nProps = filterOnNProps nForms nProps $ genN (nForms*50) (formNGen nProps (nProps+2))

-- the cnf function from exercise 3

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
cnf:: Form -> Form
cnf f | outcome /= f = cnf outcome
      | otherwise = outcome
      where outcome = (distributeORs . nnf . arrowfree) f


-- properties to test the generators themselves

-- The formGen generator only rarily generates non-satisfiable forms.
prop_generatesMostlySatisfiableForms ::  Property
prop_generatesMostlySatisfiableForms = monadicIO $ do
    s <- run $ genN 50 formGen
    assert $ length (filter satisfiable s) > 48

prop_neverMoreThanNProps ::  Property
prop_neverMoreThanNProps = monadicIO $ do
    s <- run $ genN 5000 (formNGen 5 8)
    assert $ all (\x -> length (propNames x) <= 5) s

prop_exactlyNProps :: Property
prop_exactlyNProps = monadicIO $ do
    s <- run $ genWithExactlyNProps 5000 5
    assert $ length s == 5000 && all (\x -> length (propNames x) == 5) s

-- properties to test exercise 3

prop_CNFInputUnaltered :: Property
prop_CNFInputUnaltered = monadicIO $ do
    s <- run $ generate $ formNCnjGen 5 8
    assert $ cnf s == s

-- prop_CNF fails because the CNF function is not good enough. It generates things like "-*((1==>2) (1<=>4)))" which end up causing infinite recursion.
prop_CNF :: Property
prop_CNF = monadicIO $ do
    s <- run $ genN 50 (formNGen 4 5)
    assert $ length (filter (\x -> trace (show x) (cnf x /= x)) s) > 48


main = do
    quickCheck prop_generatesMostlySatisfiableForms
    quickCheck prop_neverMoreThanNProps
    quickCheck prop_exactlyNProps
    quickCheck prop_CNFInputUnaltered
    --quickCheck prop_CNF 


-- time taken: 480m (mainly figuring out how to generate forms at all and then some more time to figure out how to generate forms with a specific amount of propositions)
-- 
