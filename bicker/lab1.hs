-- Name:	Constantijn Bicker Caarten

module Lab1 where
import Data.List
import Data.Char
import Test.QuickCheck

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Int]
primes = 2 : filter prime [3..]

-- Exercise 1
squaredSum :: Int -> Int
squaredSum n = sum(map (^2) [1..n])

squaredSumSimple :: Int -> Int
squaredSumSimple n = quot (n * (n + 1) * (2 * n + 1)) 6

checkSquaredSum :: Int -> Bool
checkSquaredSum n = squaredSum n == squaredSumSimple n

-- quickCheckResult (\(NonNegative n) -> checkSquaredSum n)

cubedSum :: Int -> Int
cubedSum n = sum(map (^3) [1..n])

cubedSumSimple :: Int -> Int
cubedSumSimple n = (quot (n * (n + 1)) 2)^2

checkCubedSum :: Int -> Bool
checkCubedSum n = cubedSum n == cubedSumSimple n

-- quickCheckResult (\(NonNegative n) -> checkCubedSum n)

-- Exercise 2 TODO make test data generator
checkPowerSetLength :: Int -> Bool
checkPowerSetLength n = length(subsequences[1..n2]) == 2^n2 where n2 = mod n 20

-- Exercise 3 TODO make test data generator
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial :: Int -> Int
factorial n = product [1..n]

-- quickCheckResult (\(NonNegative n) -> length (perms [1..n]) == factorial n)

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
consecutivePrimesProductPlusOne :: Int -> Int
consecutivePrimesProductPlusOne n = product(take n primes) + 1

-- quickCheckResult (\(NonNegative n) -> prime (consecutivePrimesProductPlusOne n))

-- Exercise 7
luhn :: Int -> Bool
luhn n = mod digitSum 10 == 0
  where
    digitSum = luhnEven (reverse (toDigits n)) 0
    -- b = checkDigit digitSum

checkDigit :: Int -> Int
checkDigit digitSum = head(reverse(toDigits (digitSum * 9)))

luhnEven :: [Int] -> Int -> Int
luhnEven [] digitSum = digitSum
luhnEven digits digitSum = luhnOdd (tail(digits)) (head(digits) + digitSum)

luhnOdd :: [Int] -> Int -> Int
luhnOdd [] digitSum = digitSum
luhnOdd digits digitSum = luhnEven (tail(digits)) (sum(toDigits(head(digits) * 2)) + digitSum)

-- source: https://stackoverflow.com/questions/24346667/haskell-converting-Int-into-list-of-digits/24346702#24346702
toDigits :: Int -> [Int]
toDigits = map digitToInt . show

-- Exercise 8
data Boy = Matthew | Peter | Jack | Arnold | Carl
          deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- accuses :: Boy -> Boy -> Bool
--
-- accusers :: Boy -> [Boy]
