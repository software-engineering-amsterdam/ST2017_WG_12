module Lab4 where

import Control.Monad
import System.Random
import SetOrd
import Test.QuickCheck
import Test.QuickCheck.Monadic

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

exerciseThree = do
          print $ show $ myUnion set1 set2
          print $ show $ myIntersect set1 set2
          print $ show $ myDifference set1 set2


