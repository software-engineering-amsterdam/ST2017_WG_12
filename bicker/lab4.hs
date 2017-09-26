module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- Exercise 3 (60 minutes)
setIntersect :: Ord a => Set a -> Set a -> Set a
setIntersect xs ys = setIntersect' xs ys emptySet

setIntersect' :: Ord a => Set a -> Set a -> Set a -> Set a
setIntersect' (Set []) _ xs = xs
setIntersect' (Set (h : t)) xs ys
    | inSet h xs = setIntersect' (list2set t) xs (insertSet h ys)
    | otherwise = setIntersect' (list2set t) xs ys

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set xs) (Set ys) = list2set (xs ++ ys)

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference xs ys = setDifference' xs ys emptySet

setDifference' :: Ord a => Set a -> Set a -> Set a -> Set a
setDifference' (Set []) _ xs = xs
setDifference' (Set (h : t)) xs ys
    | inSet h xs = setDifference' (list2set t) xs ys
    | otherwise = setDifference' (list2set t) xs (insertSet h ys)
