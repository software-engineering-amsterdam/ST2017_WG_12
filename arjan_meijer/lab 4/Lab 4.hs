module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad
import SetOrd
import Debug.Trace

-- Exercise 1
-- Time spent: 55 Minutes
-- No questions

-- Exercise 2
-- Time spent: 30 Minutes

-- source: https://hackage.haskell.org/package/checkers-0.4.7/docs/src/Test-QuickCheck-Instances-Tuple.html
{- | Generates a 2-tuple using its arguments to generate the parts. -}
(>*<) :: Gen a -> Gen b -> Gen (a,b)
x >*< y = liftM2 (,) x y

generateRelQuickCheck :: Int -> Int -> Int -> Gen (Rel Int)
generateRelQuickCheck n lower upper = do
    xs <- vectorOf n $ (choose (lower, upper)) >*< (choose (lower, upper))
    return $ sort $ nub $ xs

-- Exercise 3
-- Time spent: 15 Minutes
eTintersection, eTunion, eTdifference :: Ord a => Set a -> Set a -> Set a
eTintersection (Set xs) (Set ys) = Set ([x | x <- xs, elem x ys])
eTunion (Set xs) (Set ys) = Set ([y | y <- ys, not (elem y xs)] ++ xs)
eTdifference (Set xs) (Set ys) = Set[x | x <- xs, not (elem x ys)]
eTxOr (Set xs) (Set ys) = Set ([y | y <- ys, not(elem y xs)] ++ [x | x <- xs, not(elem x ys)]) -- (eTdifference (Set xs) (Set ys)) ++ (eTdifference (Set ys) (Set xs))

exerciseThree = do
                  let x = Set[0,1,2,3,4,5]
                  let y = Set[3,4,5,6,7,8,9,0]
                  print (eTintersection x y)
                  print (eTunion x y)
                  print (eTdifference x y)
                  print (eTxOr x y)

-- Exercise 4:
-- Time spent: 50 Minutes

-- Exercise 5:
-- Time spent: 10 Minutes
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos r = ([(y,x) | (x,y) <- r, not (elem (y,x) r)] ++ r)

exerciseFive = do
                symClos [(1,2),(2,3),(3,4)]
-- Output: [(2,1),(3,2),(4,3),(1,2),(2,3),(3,4)]

-- Exercise 6:
-- Time spent: 30 Minutes
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

trClos :: Ord a => Rel a -> Rel a
trClos r = nub (concat (map (\(x,y) -> tr' [(x,y)] r [(x,y)]) r))

tr' :: Ord a => Rel a -> Rel a -> Rel a -> Rel a
tr' a b c | x == [] || and (map (\z -> elem z c) x) = c
          | otherwise = tr' x b (c ++ x)
          where x = a @@ b


-- Exercise 7
-- Time spent: 15 Minutes
prop_sym :: Ord a => Rel a -> Bool
prop_sym r = [(x,y)|(x,y) <- r, elem (y,x) r] == r

--exerciseSevenA = do

-- prop_sym (symClos [(1,2),(4,2)])
prop_tr :: Ord a => Rel a -> Bool
prop_tr r = all (\(x,y) -> all (\(a,b) -> elem (x,b) r) [(w,z)|(w,z) <- r, w == y]) r

-- Exercise 8
-- Time spent: 15 Minutes

-- Yes, if you flip the order of the closure execution the results will
-- be different. In some cases, both closures will add relations
-- When the other closure is applied on the newly created relation
-- the relation will be updated again. When the evaluation and adding 
-- order is flipped, the results are different.
-- Example set:       [(1,2),(2,5)]
-- symClos:           [(2,1),(5,2),(1,2),(2,5)]
-- trClos $$ symClos: [(2,1),(2,2),(2,5),(5,2),(5,1),(5,5),(1,2),(1,1),(1,5)]

-- trClos:            [(1,2),(1,5),(2,5)]
-- symClos $$ trClos: [(2,1),(5,1),(5,2),(1,2),(1,5),(2,5)]

-- The difference between these relations are:
-- [(2,2),(5,5),(1,1)]