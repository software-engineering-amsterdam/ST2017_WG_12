import Control.Monad
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- source: https://hackage.haskell.org/package/checkers-0.4.7/docs/src/Test-QuickCheck-Instances-Tuple.html
{- | Generates a 2-tuple using its arguments to generate the parts. -}
(>*<) :: Gen a -> Gen b -> Gen (a,b)
x >*< y = liftM2 (,) x y

generateRelQuickCheck :: Int -> Int -> Int -> Gen (Rel Int)
generateRelQuickCheck n lower upper = do
    xs <- vectorOf n $ (choose (lower, upper)) >*< (choose (lower, upper))
    return $ sort $ nub $ xs


type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos xs = sort $ nub $ (xs ++ (map (\(x,y) -> (y,x))) xs)

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: (Ord a, Show a) => Rel a -> Rel a
trClos xs | applyTr == xs = xs
          | otherwise = trClos applyTr
          where applyTr = sort $ nub $ (xs ++ (xs @@ xs))
          
         
-- Property to test whether for all (x,y) in a relation, (y,x) exists after applying symClos         
prop_symmetric :: Property
prop_symmetric = monadicIO $ do
    rel <- run $ generate $ generateRelQuickCheck 10 1 5
    assert $ all (\(x,y) -> elem (y,x) (symClos rel)) rel
    
-- This property only checks for the first pass of transitive.
-- e.g. trClos [(1,2),(2,3),(3,4),(4,5)] generates an (1,4) but this is not checked, only (1,3) and (2,4).
prop_transitive :: Property
prop_transitive = monadicIO $ do
    rel <- run $ generate $ generateRelQuickCheck 10 1 5
    assert $ all (\x -> elem x (trClos rel)) [(x,z) | (x,y) <- rel, (w,z) <- rel, y == w]

main = do
    quickCheck prop_symmetric
    quickCheck prop_transitive

-- time spent: 120m

-- output:

-- *Main> main
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
