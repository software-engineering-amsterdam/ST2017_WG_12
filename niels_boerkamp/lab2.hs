module Lab2 where

import Data.List
import Data.Char 
import System.Random
import Test.QuickCheck

forall = flip all

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


-- Exercise 1
-- 60 minutes
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

replace :: [Int] -> Int -> Int -> [Int]
replace xs n newValue = take n xs ++ [newValue] ++ drop (n + 1) xs

 
getIndex :: Float -> Int
getIndex x | x < 0.25 = 0
           | x >= 0.25 && x < 0.5 = 1
           | x >= 0.5 && x < 0.75 = 2
           | x >= 0.75 = 3

probsTest :: [Float] -> [Int] -> [Int]
probsTest [] counters = counters
probsTest (x:xs) counters = probsTest xs newCounter where
                            newValue = counters !! (getIndex x)
                            newCounter = replace counters (getIndex x) newValue

probsTest2 = do 
            x <- probs 100
            print $ show $ probsTest x [0,0,0,0]

-- Exercise 2
-- 40 minutes
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | prop_no_triangle x y z = NoTriangle 
               | prop_equilateral x y z = Equilateral
               | prop_rectangular x y z = Rectangular
               | prop_isosceles x y z = Isosceles
               | otherwise = Other

prop_rectangular, prop_no_triangle, prop_equilateral :: Integer -> Integer -> Integer -> Bool
prop_rectangular x y z = (x^2 + y^2 == z^2) || (z^2 + y^2 == x^2) || (z^2 + x^2 == y^2)
prop_no_triangle x y z = (x + y < z) || (x + z < y) || (y + z < x)
prop_equilateral x y z = x == y && y == z
prop_isosceles x y z = x == y || y == z || x == z
  
-- Exercise 3
-- 15 minutes
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

a, b, c, d :: Int -> Bool
a x = even x && x > 3
b x = even x || x > 3
c x = (even x && x > 3) || even x
d x = (even x && x > 3) || even x

-- Exercise 3a
-- stronger [(-10)..10] a even
-- stronger [(-10)..10] b even
-- stronger [(-10)..10] c even
-- stronger [(-10)..10] even d

-- Exercise 3b
-- sortByStrongest :: [Int] -> [(Int -> Bool)] -> [(Int -> Book)]
-- sortByStrongest list

-- Exercise 4
-- 15 minutes
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = elem xs (perms ys)

-- Test cases
permTest1 = isPermutation [1..100] [1..100] -- should pass -> Passes
permTest2 = isPermutation [1,2,3,4] [1,2,4,3] -- should pass -> Passes
permTest3 = isPermutation ([]::[Int]) ([]::[Int]) -- should pass -> Passes
permTest4 = isPermutation [1] [] -- should fail -> Fails

-- Test properties
sameLength :: Eq a => [a] -> [a] -> Bool
sameLength xs ys = length (group xs) == length (group ys)

data NonNegativeSmall = NonNegativeSmall Integer deriving Show
instance Arbitrary NonNegativeSmall where
    arbitrary = fmap NonNegativeSmall (choose (1, 1000))
exerciseFour = do quickCheckResult (\(NonNegativeSmall n) -> isPermutation [0..n] [0..n])

-- Exercise 5
-- 35 minutes
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && and (zipWith (/=) xs ys)

derangements :: Eq a => [a] -> [[a]]
derangements xs = filter (isDerangement xs) (perms xs)

-- Exercise 6
-- 60 minutes
-- Specification of ROT13
-- The ROT13 algoritmn replaces all characters of a string with a char 13 places further in the alphabet.
-- Since the alphabet has 26 characters the same function can be used for encryption and decryption.
-- https://en.wikipedia.org/wiki/ROT13
rot13 :: String -> String
rot13 str = map rot13' str

rot13' :: Char -> Char
rot13' x | isUpper x = chr (((((ord x)- 65) + 13)`mod` 26) + 65)
		 | isLower x = chr (((((ord x)- 97) + 13)`mod` 26) + 97)
		 | otherwise = x

data RandomString = RandomString String deriving Show
instance Arbitrary RandomString where
    arbitrary = fmap RandomString (listOf $ elements (map chr ([0..160])))

exercise6 = do quickCheckResult(\(RandomString x) -> rot13 (rot13 x) == x)

--ibanDutch :: String -> Bool

prop_dutchIBANLength :: String -> Bool
prop_dutchIBANLength x = length  x == 18

toIBANDigit :: Char -> Int
toIBANDigit x = (ord x) - 55

rearrange :: [Char] -> [Char]
rearrange x = (snd (splitAt 4 x)) ++ (take 4 x)

mapIf pred f = map (\x -> if pred x then f x else x)

