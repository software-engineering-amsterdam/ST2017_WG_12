-- Arjan Meijer, 11425555
-- University of Amsterdam

-- Sources used:
  -- [1] https://stackoverflow.com/questions/27407773/sum-of-squares-using-haskell
  -- For haskell syntax, usage of map
  
  -- [2] https://wiki.haskell.org/List_comprehension
  -- For haskell syntax, looping through arrays with the usage of 'for each element in array' [ x | x <- [], doSomething x]

  -- [3] https://stackoverflow.com/questions/18408292/haskell-quickcheck-generate-random-data-for-function-with-many-input-variables
  -- For quickcheck number input
  
  -- [4] https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms
  -- For random number generator
module Lab1 where
import Data.List
import Test.QuickCheck
import Debug.Trace
import System.Random
import Control.Monad (replicateM) -- source [4]

-- Prime methods

prime :: Integer -> Bool
prime n = n > 1 && all(\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

primesTo :: Integer -> [Integer]
primesTo x = 2 : filter prime [3..x]

primesFromTo :: Integer -> Integer -> [Integer]
primesFromTo x y = 2 : filter prime [x..y]

nextPrime :: Integer -> Integer
nextPrime n = if prime n then n else nextPrime(n + 1)

-- QuickTest number generator 
-- SOURCE [3]
data TestNumbers = TestNumbers Integer deriving Show
instance Arbitrary TestNumbers where arbitrary = fmap TestNumbers (choose (1, 25))

data LimitedTestNumbers = LimitedTestNumbers Integer deriving Show
instance Arbitrary LimitedTestNumbers where arbitrary = fmap LimitedTestNumbers (choose (1, 25))

-- Exercise 1
-- Source [1]
exercise2Test :: Integer -> Bool
exercise2Test x = (sum $ map (^2) [0..x]) == (x * ((x + 1) * (2*x + 1))) `div` 6

exercise3Test :: Integer -> Bool
exercise3Test x = (sum $ map (^3) [0..x]) == ((x * (x + 1)) `div` 2)^2

-- Command used for testing: quickCheckResult(\(TestNumbers x) -> exercise2Test x)
--                           quickCheckResult(\(TestNumbers x) -> exercise3Test x)

-- Exercise 2
exercise4Test :: Integer -> Bool
exercise4Test x = length (subsequences [1..x]) == 2^x

-- As long as we concider the 'Subsequences' method to be correct, the test is simple,
-- If it is unknown if the subsequences method is correct, testing will be a lot more difficult

-- I am testing the subsequences method since it is a fact that the length 
-- of the subsequences should be 2^n. Therefore this method tests if the 
-- subsequences method returns the correct (suspected) value
-- Command used for testing: quickCheckResult(\(TestNumbers x) -> exercise4Test x)

-- Exercise 3
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
   insrt x [] = [[x]]
   insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- Helper function: factorial
factorial :: Integer -> Integer
factorial x | x < 2 = 1
            | otherwise = x * factorial (x - 1)   

exercise5Test :: Integer -> Bool
exercise5Test x = toInteger (length (perms [1..x])) == (factorial x)

-- As long as we only check the length of the perms method and not the returned
-- values the test won't be hard. But the testing will be harder
-- if we have to check all individual results.

-- We are testing if the amount of arrays that are produced by perms method 
-- is the same value as the result of the factorial method, if we concider the 
-- factorial method to be correct, we can say that we are testing if the perms
-- method returns the correct amount of arrays

-- Command used for testing: quickCheckResult(\(LimitedTestNumbers x) -> exercise5Test x)


-- Exercise 4
reversablePrime :: [Integer]
reversablePrime = rPrime [] (primesTo 10000)

-- Helper method: collects primes that are reversible from a given array
rPrime :: [Integer] -> [Integer] -> [Integer]
rPrime xs (y:ys) | prime (reverseInt y) = rPrime (y:xs) ys
                 | otherwise = rPrime xs ys
rPrime xs [] = xs

-- Helper method: Reverses and integer
reverseInt :: Integer -> Integer
reverseInt x | x < 0 = 0 - (read . reverse . tail . show $ x)
             | otherwise = read . reverse . show $ x

-- This method can be tested by re-doing the requested calculation on the results

-- Exercise 5
e5 :: Integer -> Integer -> [Integer] -> [Integer]
e5 p y zs   
 | toInteger (length zs) > y = e5 p y (take ((length zs) - 1) zs) 
 | toInteger (length zs) < y = e5 (nextPrime (p + 1)) y (p:zs)
 | toInteger (length zs) == y && prime (sum zs) = zs
 | otherwise = e5 p y (take ((length zs) - 1) zs) 

exercise5 = e5 2 101 []

-- An answer should always be tested if that is possible
-- To test this answer we have to check three things,
-- I.   The given are primes and should be consecutive
-- II.  The sum must be correct
-- III. The sequence should start as low as possible

-- Exercise 6
e6 :: [Integer] -> Integer
e6 (x:xs) | prime ((product (x:xs)) + 1) = e6 ((nextPrime (x + 1)) : (x:xs))
                 | otherwise = x
e6 [] = e6 [2]

exercise6 = e6 []
-- The smallest prime that disproves the statement is 13
-- Tested by running command exercise6

-- Exercise 7
luhn :: Integer -> Bool
luhn x = (y + ((y * 9) `mod` 10)) `mod` 10 == 0 where y = (l' x 0)

-- Helper method: luhn
l' :: Integer -> Integer -> Integer
l' x y | x == 0 = y
       | otherwise = l' (x `div` 100) (y + (intTo1Int ((x `mod` 10) * 2)) + ((x `div` 10) `mod` 10))

-- Helper method: int of multiple characters to 1 int
intTo1Int :: Integer -> Integer
intTo1Int x | x < 10 = x
            | otherwise = (x `mod` 10) + intTo1Int (x `div` 10)

-- Check methods			
isAmericanExpress, isMaster :: Integer -> Bool
isAmericanExpress x = length (show x) == 15 && luhn x && y == 34 || y == 37 where y = read (take 2 (show x))
isMaster x = length (show x) == 16 && luhn x && ((y >= 2221 && y <= 2720) || (z >= 51 && z <= 55)) where y = read (take 4 (show x))
                                                                                                         z = read (take 2 (show x))
isVisa x = (y == 13 || y == 16 || y == 19) && read (take 1 (show x)) == 4 where y = length (show x)

-- Test methods
-- SOURCE for random numbergeneration [4]
getRandomAmericanExpressCard :: Integer
getRandomAmericanExpressCard = randAmericanExpress 34

randAmericanExpress :: Integer -> Integer
randAmericanExpress x | x < 15 = (show x) ++ show(randomRIO(0,9 :: Integer))
                      | otherwise = read x


-- Exercise 8
data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq, Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses x y | x == Matthew = y /= Matthew && y /= Carl
            | x == Peter = y == Matthew || y == Jack
            | x == Jack = not (accuses Matthew y) && not (accuses Peter y)
            | x == Arnold = accuses Matthew y /= accuses Peter y
            | x == Carl = not (accuses Arnold y)

accusers :: Boy -> [Boy]
accusers x = accusers' x boys []

-- Helper method to find accusers
accusers' :: Boy -> [Boy] -> [Boy] -> [Boy]
accusers' b (x:xs) ys | accuses x b = accusers' b xs (x:ys)
                      | otherwise = accusers' b xs ys
accusers' b [] ys = ys

-- Used source [2]
guilty, honest :: [Boy]
guilty = [ x | x <- boys, length (accusers x) == 3]
honest = [ x | x <- boys, accuses x (head guilty)]

-- Guilty : Jack
-- Honest: Matthew, Peter & Carl