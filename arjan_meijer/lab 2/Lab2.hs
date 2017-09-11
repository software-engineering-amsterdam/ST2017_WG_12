module Lab2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Inspiration: https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
upNth :: Int -> [Int] -> [Int]
upNth n []     | n == 0 = [1]
               | otherwise = 0:upNth (n-1) []
upNth n (x:xs) | n == 0 = (x + 1):xs
               | otherwise = x:upNth (n-1) xs

-- Exercise 1
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

checkProbs :: [Int] -> [Float] -> [Int]
checkProbs count (x:xs) | x > 0.75 = checkProbs (upNth 3 count) xs
                        | x > 0.5 = checkProbs (upNth 2 count) xs
                        | x > 0.25 = checkProbs (upNth 1 count) xs
                        | otherwise = checkProbs (upNth 0 count) xs
checkProbs count [] = count

exerciseOne = do 
                x <- probs 100000
                putStrLn $ show $ (checkProbs [0,0,0,0] x)

-- Exercise 1 results:
--        [2505,  2510, 2482, 2503]
--        [2458,  2476, 2488, 2578]
--        [2536,  2517, 2508, 2439]
--        [2560,  2478, 2440, 2522]
-- TOTAL: [10059, 9981, 9918, 10042]
-- There are no big differences over the sum of four test,
-- But in this test set there is a bigger chance that a number 
-- falls in the first or the last quartile

-- Resulst of a test with more numbers:
--        [25117,  24975,  24922, 24986]
--        [24760,  25129,  25067, 25044]
--        [25288,  24947,  24933, 24832]
--        [24856,  25080,  24961, 25103]
-- TOTAL: [100021, 100131, 99883, 99965]
-- There are also no big defferences in the sum of these four tests,
-- Therefore we can assume that the method works

-- Exercise 2
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | checkRectangular x y z = Rectangular
               | otherwise = Other

checkRectangular :: Integer -> Integer -> Integer -> Bool
checkRectangular x y z | x >= y && x >= z = x^2 == y^2 + z^2
                       | y >= x && y >= z = y^2 == x^2 + z^2
                       | otherwise = z^2 == x^2 + y^2