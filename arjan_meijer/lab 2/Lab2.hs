module Lab2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Inspiration: https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
upNth :: Int -> [Int] -> [Int]
upNth n []     | n == 0 = [1]
               | otherwise = 0:upNth (n-1) []
upNth n (x:xs) | n == 0 = (x + 1):xs
               | otherwise = x:upNth (n-1) xs
  
forall = flip all

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Exercise 1
-- 45 min
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
-- 10 min
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | isNotTriangle x y z = NoTriangle
               | isEquilateral x y z = Equilateral
               | isIsosceles x y z = Isosceles
               | isRectangular x y z = Rectangular
               | otherwise = Other

isNotTriangle, isEquilateral, isIsosceles, isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular x y z | x >= y && x >= z = z^2 == y^2 + x^2
                    | y >= x && y >= z = z^2 == x^2 + y^2
                    | otherwise = y^2 == x^2 + z^2
isNotTriangle x y z = x + y < z || x + z < y || y + z < x
isEquilateral x y z = x == y && y == z
isIsosceles x y z = x == y || y == z || x == z

-- Exercise 3
-- 45 min

-- A
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs p q = stronger xs q p

-- Workshop 3 exercises
e1, e2, e3, e4 :: Int -> Bool
e1 x = even x && x > 3
e2 x = even x || x > 3
e3 x = (even x && x > 3) || even x
e4 x = (even x && x > 3) || even x

exerciseThreeA = do
                 print $ "The first is " ++ if stronger [(-10)..10] e1 even then "stronger" else "weaker"
                 print $ "The first is " ++ if stronger [(-10)..10] e2 even then "stronger" else "weaker"
                 print $ "The first is " ++ if stronger [(-10)..10] e3 even then "stronger" else "weaker"
                 print $ "The first is " ++ if stronger [(-10)..10] e4 even then "stronger" else "weaker"

-- B
isStronger :: [a] -> (Int, (a -> Bool)) -> [(Int, (a -> Bool))] -> Bool -> Bool
isStronger xs x (y:ys) c | c == False = False
                         | otherwise = isStronger xs x ys (stronger xs (snd x) (snd y))
isStronger xs x [] c = c

orderStrongest :: [a] -> [(Int, (a -> Bool))] -> [(Int, (a -> Bool))] -> [Int]
orderStrongest xs (y:ys) res | isStronger xs y ys True = orderStrongest xs ys (y:res)
                             | otherwise = orderStrongest xs (ys ++ [y]) res 
orderStrongest xs [] res = [i | (i,_) <- res]

exerciseThreeB = orderStrongest [(-10)..10] (zip [0..] [e1,e2,e3,e4]) []
-- Result: [1,3,2,0] -> [e2, e4, e3, e1]

-- Exercise 4
-- 35 min

-- A
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation (x:xs) ys | elem x ys = isPermutation xs (removeFirst x ys)
                        | otherwise = False
isPermutation [] ys = ys == []

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst x (y:ys) | x == y = ys
                     | otherwise = removeFirst x (ys ++ [y])

-- B
permTest1 :: Eq a => [a] -> [a] -> Bool
permTest1 xs ys = length xs == length ys

--permutationTest1 = isPermutation ([]::[Int]) ([]::[Int])
--permutationTest2 = isPermutation [0..50] [0..50]

-- Exercise 5
-- 90 min

isDerangement :: [Int] -> [Int] -> Bool
isDerangement (x:xs) (y:ys) | x == y = False
                            | otherwise = isDerangement xs ys
isDerangement [] ys = ys == []
isDerangement xs [] = xs == []

deran :: [Int] -> [[Int]]
deran n = filter (\x -> isDerangement x n) (permutations n)

-- Exercise 6
-- 00 min
-- Specification
-- The input length is the same as the output length
-- Only works on the 26 letters of the 'basic Latin alphabet'
-- Is its own inverse

rot13 :: String -> String
rot13 s = [flipRot13 x | x <- s]

flipRot13 :: Char -> Char
flipRot13 c | (x <= ord 'Z' && x > ord 'N') || (x <= ord 'z' && x > ord 'n') = chr (x - 13)
            | (x <= ord 'N' && x >= ord 'A') || (x <= ord 'n' && x >= ord 'z') = chr (x + 13)
            | otherwise = c
             where x = ord c

-- Exercise 7
ibanLength :: String -> Bool
ibanLength s | x == "GB" = length s == 22
             | x == "GR" = length s == 27
             | x == "DE" = length s == 22
             | x == "SA" = length s == 24
             | x == "CH" = length s == 21
             | x == "RO" = length s == 24
              where x = take 2 s

ibanToStr :: String -> String
ibanToStr n = concat [ibanCharToStr x | x <- y] where y = (reverse (take ((length n)-4) (reverse n))) ++ take 4 n

ibanCharToStr :: Char -> String
ibanCharToStr c | (ord c) >= ord 'A' && (ord c) <= ord 'Z' = show (((ord c) - (ord 'A')) + 10)
                | otherwise = [c]

iban :: String -> Bool
iban n = ibanLength n && ((read (ibanToStr n) :: Integer) `mod` 97) == 1

ibanManualTest = do
                  print (iban "GR1601101250000000012300695")
                  print (iban "GB29RBOS60161331926819")
                  print (iban "DE89370400440532013000")
                  print (iban "SA0380000000608010167519")
                  print (iban "CH9300762011623852957")
                  print (iban "RO49AAAA1B31007593840000")

