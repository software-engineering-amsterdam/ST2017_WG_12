module Lab6

where

import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Lecture6 hiding (exM, composites)

-- Exercise 1 (15 minutes)

exM :: Integer -> Integer -> Integer -> Integer
exM b e m = exM' b e 0 m 1

exM' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
exM' b e e' m c
    | e' < e = exM' b e (e' + 1) m (b * c `mod` m)
    | otherwise = c

-- Exercise 2 ()

-- Exercise 3 (15 minutes)

composites :: [Integer]
composites = [x | x <- [1..], (length $ factors x) > 1]

-- Exercise 4 (90 minutes)
primalityTest f (x:xs) = do
    r <- f x
    if r then print x else return ()
    primalityTest f xs

-- primalityTest' f (x:xs) k = do
--     until (f x)
--     return expression

testFermat n = do
    x <- primeTestF (composites!!n)
    if x then return (composites!!n)
    else testFermat $ succ n

data RandomComposite = RandomComposite Integer deriving Show
instance Arbitrary RandomComposite where
    arbitrary = fmap RandomComposite (elements [1..20]) -- replace with composites

generateComposite :: Int -> IO Integer
generateComposite 0 = do return 0
generateComposite n = generate $ elements $ take n composites

prop_test :: Int -> Property
prop_test n = monadicIO $ do
    composite <- run $ generateComposite n
    r <- run $ primeTestF composite
    assert $ not r

-- abc :: [Bool] -> Integer -> IO [Bool]
-- abc xs 100 = xs
-- abc xs n = do
--     x <- testFermat 0
--     return (abc (x:xs) (succ n))

testsFermat k n = do
    x <- primeTestsF k (composites!!n)
    if x then return (composites!!n)
    else testsFermat k $ succ n

-- Exercise 5 ()
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
    k <- [2..],
    prime (6*k+1),
    prime (12*k+1),
    prime (18*k+1) ]

-- Exercise 6 ()
