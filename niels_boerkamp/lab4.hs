module Lab4 where

import Lecture4
import Lab3
import SetOrd
import Data.List

-- Exercise 1
-- Time spent: 60

-- Exercise 2
-- Time spent:


-- Exercise 3
-- Time spent: 20 minutes
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

-- Exercise 4


-- Exercise 5
-- Time spent: 25 minutes
type Rel a = [(a,a)]

symClos :: Ord a  => Rel a -> Rel a
symClos xs = [(y,x) | (x,y) <- xs, not (elem (y,x) xs)] ++ xs

exerciseFive = do
          print $ show $ symClos [(1,2), (2,1), (1,1), (3,1)]

-- Exercise 6
-- Time spent: 60 minutes
infixr 5 @@
 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]
  
trClos :: Ord a => Rel a -> Rel a
trClos xs = if length xs == length next
            then sort xs
            else trClos next
                where next = (nub (xs ++ (xs @@ xs)))

exerciseSix = do
            print $ show $ [(1,2),(2,3),(3,4)]
            print $ show $ trClos [(1,2),(2,3),(3,4)]

-- Exercise 7
-- Time spent: 15
prop_transitive :: Ord a => Rel a -> Bool
prop_transitive xs = length xs == length (nub (xs ++ (xs @@ xs)))

prop_symmetric :: Ord a => Rel a -> Bool
prop_symmetric xs = all (\ (x,y) -> (elem (y,x) xs)) xs

-- Exercise 8
-- Time spent: 5 minutes
-- Proof by contradiction. The order in which these functions
-- are executed matters.

symClosAndTrClos = symClos (trClos [(1,2),(2,3),(3,4)])
-- Result: [(2,1),(3,1),(4,1),(3,2),(4,2),(4,3),(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

trClosAndSymClos = trClos (symClos [(1,2),(2,3),(3,4)])
-- Result: [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)]
