module Lab1 where
import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

primesBelow :: Integer -> [Integer]
primesBelow 0 = []
primesBelow x = filter prime [0..x]

data NonNegativeSmall = NonNegativeSmall Integer deriving Show
instance Arbitrary NonNegativeSmall where
    arbitrary = fmap NonNegativeSmall (choose (1, 10))

-- Exercise 1a
-- 5 minutes
sumNumbersSquared :: Integer -> Integer
sumNumbersSquared x = sum( map (^2) [0..x])

exercise1a :: Integer -> Bool
exercise1a x = sumNumbersSquared x == (x * (x + 1) * (2 * x + 1)) `div` 6

-- quickCheckResult (\(NonNegative n) -> proof1 n)

-- Exercise 1b
-- 5 minutes
sumNumbersCubed :: Integer -> Integer
sumNumbersCubed x = sum (map(^3) [0..x])

exercise1b :: Integer -> Bool
exercise1b x = sumNumbersCubed x == ((x * (x + 1)) `div` 2) ^ 2

-- quickCheckResult (\(NonNegative n) -> proof2 n)


-- Exercise 2
-- 10 minutes
-- It isn't hard to test when we make the assumption the subsequences function
-- has been implemented correctly.
-- If we want to do a complete test we should also look at the result of the subsequences function.

-- We are now testing the fact that the length of the powerset (generated by subsequences) has a
-- size of 2^x where x is |A|. 
exercise2 :: Integer -> Bool
exercise2 x = length (subsequences [1..x]) == 2^x
 

-- Exercise 3
-- 40 minutes
-- The answer of exercise 2 also applies to exercise 3. If we assume the perms functions
-- is implemented correctly it isn't hard to test this function. But if we also have to test
-- the permutations it becomes more difficult. 
factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

exercise3 :: Integer -> Bool
exercise3 x = factorial x == toInteger(length(perms [1..x]))
-- quickCheckResult(\(NonNegativeSmall x) -> exercise3 x)
  
-- Exercise 4
-- 50 minutes
-- To test this function it would gather a lot of testscenarios. The testscenarios will be verified 
-- with function which are proven to be  implemented correctly like the prime function.
reversal :: Integer -> Integer
reversal = read . reverse . show

reversalPrime :: Integer -> Bool
reversalPrime x = prime x && prime (reversal x)

findReversalPrimes :: Integer -> [Integer]
findReversalPrimes x = filter reversalPrime (primesBelow x)

exercise4 = findReversalPrimes 20

-- Exercise 5
-- 70 minutes
-- This function can easily be tested. We need to look at the list of numbers and check if they are all prime.
-- When we have found out that they are prime, we can look at the sum. This number should also be prime.

nextPrime :: Integer -> Integer
nextPrime x = if prime (x + 1) then (x + 1) else nextPrime (x + 1)

-- Max length -> start prime -> List of primes
primeList :: Integer -> Integer -> [Integer] -> [Integer]
primeList maxLen start [] = primeList maxLen start [nextPrime start]
primeList maxLen start xs | toInteger(length (xs)) < maxLen = primeList maxLen start (nextPrime (head xs) : xs)
                          | otherwise = xs
 
consecutivePrimes :: Integer -> [Integer] -> [Integer]
consecutivePrimes startprime xs | prime (sum xs) && length xs == 101 = xs
                                | otherwise =consecutivePrimes (startprime + 1) (primeList 101 startprime [])

exercise5 = consecutivePrimes 0 []
	
-- Exercise 6
-- 20 minutes
-- Smallest counterexample: [13,11,7,5,3,2]
followingConjuncture :: Integer -> [Integer]
followingConjuncture len | not (prime ((product xs) + 1)) = xs
						 | otherwise = followingConjuncture (len + 1)
						 where xs = primeList len 0 []
exercise6 = followingConjuncture 2


-- Exercise 7


-- Exercise 8

-- Euler problem 10
euler10 = sum (primesBelow 2000000)



