module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- Exercise 1 (65 minutes)

-- Exercise 2 (10 minutes)
-- randomSet :: Int -> Int -> Set Int
-- randomSet n m = take (randomRIO (n, m)) (generate $ shuffle [n..m])

-- data RandomSetInt = RandomSetInt [Integer] deriving Show
-- instance Arbitrary RandomSetInt where
--     arbitrary = fmap RandomSetInt (listOf $ elements (generate $ shuffle [1..512]))

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

-- Exercise 4 (90 minutes)

-- Exercise 5 (20 minutes)
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos r = sort (nub (r ++ (map (\ (a,b) -> (b,a)) r)))

-- Exercise 6 (60 minutes)
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Based on: https://stackoverflow.com/questions/19212558/transitive-closure-from-a-list-using-haskell/19214140#19214140
trClos :: Ord a => Rel a -> Rel a
-- trClos r
--     | r == s = sort r
--     | otherwise = trClos s
--     where s = nub $ r ++ (r @@ r)

trClos r = until (\ x -> x == f x) f r
    where f = (\ x -> sort (nub (x ++ (x @@ x))))

-- fp :: Eq a => (a -> a) -> a -> a
-- fp f = until (\ x -> x == f x) f

-- With fp
-- trClos r = fp (\ x -> sort (nub (x ++ (x @@ x)))) r

-- Exerercise 7 (65 minutes)
-- prop_sym :: Rel a -> Bool
prop_sym r = and (map (\ (a,b) -> elem (b,a) r) r)

prop_tr r = r == nub [(a,c) | (a,b) <- r, (b',c) <- r, b==b']

-- randomRel :: Int -> Rel a
-- randomRel 0 = []
-- randomRel n = vectorOf n (Positive x, Positive y)

-- data RandomRel = RandomRel Rel deriving Show
-- instance Arbitrary RandomRel where
--     arbitrary = vectorOf (choose (0,128) RandomTuple)

-- data RandomTuple = RandomTupleInt (Gen Int, Gen Int) deriving Show
-- instance Arbitrary RandomTuple where
--     arbitrary = (choose (0,128), choose (0,128))

-- Exercise 8 (15 minutes)
-- A contradiction shows that there is a difference between the symmetric
-- closure of the transitive closure of a relation R and the transitive
-- closure of the symmetric closure of R
exercise8 :: Bool
exercise8 = do
    let example = [(1,2),(2,3),(3,4)]
    let x = symClos (trClos example)
    let y = trClos (symClos example)
    x == y
