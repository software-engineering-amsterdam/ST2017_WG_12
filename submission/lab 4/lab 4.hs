module Lab4 where

import Control.Monad
import System.Random
import SetOrd
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.List

-- Exercise 1
-- Time spent:
-- Arjan 		55 minutes
-- Constantijn 	45 minutes
-- Michael 		45 minutes
-- Niels		50 minutes

-- Exercise 2
-- Time spent:
-- Arjan 		30 minutes
-- Constantijn 	10 minutes
-- Michael 		150 minutes
-- Niels		100 minutes

-- We have chosen Michael implementation, since it provides the best results.

allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = notElem x xs && allUnique xs

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (x:[]) = True
isSorted (x:y:xs) = x < y && isSorted (y:xs)

getValuesFromSet :: Set Int -> [Int]
getValuesFromSet (Set a) = a

generateList n lower upper = replicateM n $ randomRIO (lower,upper :: Int)

turnListIntoSet :: IO [Int] -> IO (Set Int)
turnListIntoSet xs = do
    xs' <- xs
    return $ list2set xs'

generateSetQuickCheck :: Int -> Int -> Int -> Gen (Set Int)
generateSetQuickCheck n lower upper = do
    xs <- vectorOf n $ choose (lower, upper)
    return $ list2set xs

prop_noDuplicates :: Gen (Set Int) -> Property
prop_noDuplicates gen = monadicIO $ do
    set <- run $ generate gen
    assert $ allUnique $ getValuesFromSet set

prop_orderedSet :: Gen (Set Int) -> Property
prop_orderedSet gen = monadicIO $ do
    set <- run $ generate gen
    assert $ isSorted $ getValuesFromSet set

exerciseOne = do
    genSet <- turnListIntoSet $ generateList 10 1 100
    print genSet
    quickCheck $ prop_noDuplicates $ generateSetQuickCheck 10 1 100
    quickCheck $ prop_orderedSet $ generateSetQuickCheck 10 1 100

-- Exercise 3
-- Arjan 		15 minutes
-- Constantijn 	60 minutes
-- Michael 		90 minutes
-- Niels		20 minutes

myUnion :: Ord a => Set a -> Set a-> Set a
myUnion (Set xs) (Set ys) = list2set (xs++ys)

myIntersect :: Ord a => Set a -> Set a -> Set a
myIntersect (Set xs) (Set ys) = Set [x | x <- xs, elem x ys]

myDifference :: Ord a => Set a -> Set a -> Set a
myDifference (Set xs) (Set ys) = Set [x | x <- xs, not (elem x ys)]

set1 = list2set [1,2,3,4,5,6]
set2 = list2set [4,5,6,7,8,9]

setLength :: Set Int -> Int
setLength (Set xs) = length xs          
          
prop_sameInput :: Property
prop_sameInput = monadicIO $ do
    set <- run $ generate $ generateSetQuickCheck 10 1 100
    assert $ (myIntersect set set) == set 
    assert $ (myUnion set set) == set
    assert $ (myDifference set set) == Set []
    
prop_length :: Property
prop_length = monadicIO $ do
    set1 <- run $ generate $ generateSetQuickCheck 10 1 100
    set2 <- run $ generate $ generateSetQuickCheck 10 1 100
    assert $ setLength (myIntersect set1 set2) <= min (setLength set1) (setLength set2)
    assert $ setLength (myUnion set1 set2) >= max (setLength set1) (setLength set2)
    assert $ setLength (myDifference set1 set1) <= min (setLength set1) (setLength set2)

exerciseThree = do
          print $ show $ myUnion set1 set2
          print $ show $ myIntersect set1 set2
          print $ show $ myDifference set1 set2
          quickCheck $ prop_sameInput
          quickCheck $ prop_length
          
-- Exercise 4
-- Arjan 		30 minutes
-- Constantijn 	0 minutes
-- Michael 		0 minutes
-- Niels		60 minutes

-- No questions yet

-- Exercise 5
-- Arjan 		10 minutes
-- Constantijn 	20 minutes
-- Michael 		15 minutes
-- Niels		25 minutes

-- We have chosen Arjans approach since he completed tbe exercise with the least code.
-- However, we have added 'sort' to his implementation. Sorting the list was part of Michaels and Constantijns work.
-- Since this was a good addition we have also implemented in our final solution.

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos r = sort ([(y,x) | (x,y) <- r, not (elem (y,x) r)] ++ r)

exerciseFive = do
                symClos [(1,2),(2,3),(3,4)]

-- Exercise 6
-- Arjan 		30 minutes
-- Constantijn 	61 minutes
-- Michael 		30 minutes
-- Niels		60 minutes

-- The input specification is an orderd list and the same goes for the output. Therefore, the where clause 
-- needs to produce sorted output.

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: (Ord a, Show a) => Rel a -> Rel a
trClos xs | applyTr == xs = xs
          | otherwise = trClos applyTr
          where applyTr = sort $ nub $ (xs ++ (xs @@ xs))
          
-- Exercise 7
-- Arjan 		15 minutes
-- Constantijn 	65 minutes
-- Michael 		120 minutes
-- Niels		15 minutes

prop_tr :: Ord a => Rel a -> Bool
prop_tr r = all (\(x,y) -> all (\(a,b) -> elem (x,b) r) [(w,z)|(w,z) <- r, w == y]) r

-- Property to test whether for all (x,y) in a relation, (y,x) exists after applying symClos         
prop_symmetric :: Property
prop_symmetric = monadicIO $ do
    rel <- run $ generate $ generateRelQuickCheck 10 1 5
    assert $ all (\(x,y) -> elem (y,x) (symClos rel)) rel
    
-- Exercise 8
-- Time spent: 5 minutes
-- Proof by contradiction. The order in which these functions
-- are executed matters.

symClosAndTrClos = symClos (trClos [(1,2),(2,3),(3,4)])
-- Result: [(2,1),(3,1),(4,1),(3,2),(4,2),(4,3),(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

-- This results in not only a symmetic and transative closure, but also reflexive
trClosAndSymClos = trClos (symClos [(1,2),(2,3),(3,4)])
-- Result: [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)]
    

