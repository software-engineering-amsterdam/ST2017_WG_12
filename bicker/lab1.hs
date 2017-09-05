-- Name:	Constantijn Bicker Caarten

module Lab1 where
import Data.List
import Test.QuickCheck

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Int]
primes = 2 : filter prime [3..]

-- Exercise 1
-- TODO fix test data generator
squaredSum :: Int -> Int
squaredSum n = sum(map (^2) [1..n])

squaredSumSimple :: Int -> Int
squaredSumSimple n = quot (n * (n + 1) * (2 * n + 1)) 6

checkSquaredSum :: Int -> Bool
checkSquaredSum n = squaredSum npos == squaredSumSimple npos where npos = abs(n)

cubedSum :: Int -> Int
cubedSum n = sum(map (^3) [1..n])

cubedSumSimple :: Int -> Int
cubedSumSimple n = (quot (n * (n + 1)) 2)^2

checkCubedSum :: Int -> Bool
checkCubedSum n = cubedSum npos == cubedSumSimple npos where npos = abs(n)

-- Exercise 2
checkPowerSetLength :: Int -> Bool
checkPowerSetLength n = length(subsequences[1..npos]) == 2^npos where npos = mod (abs(n)) 20

-- Exercise 3

-- Exercise 4
reversal :: Int -> Int
reversal = read . reverse. show

-- Exercise 5
consecutivePrimes :: Int -> Int -> [Int]
consecutivePrimes start n = take n (drop start primes)

findConsecutivePrimesPrimeSum :: Int -> Int
findConsecutivePrimesPrimeSum n = findConsecutivePrimesPrimeSum' 0 n

findConsecutivePrimesPrimeSum' :: Int -> Int -> Int
findConsecutivePrimesPrimeSum' start n
  | prime $ sum(consecutivePrimes start n) = sum(consecutivePrimes start n)
  | otherwise = findConsecutivePrimesPrimeSum' (start + 1) n

-- Exercise 6

-- Exercise 7

-- Exercise 8
