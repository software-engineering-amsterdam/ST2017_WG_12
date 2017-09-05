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

-- quickCheckResult (\(Positive n) -> checkSquaredSum n)

cubedSum :: Int -> Int
cubedSum n = sum(map (^3) [1..n])

cubedSumSimple :: Int -> Int
cubedSumSimple n = (quot (n * (n + 1)) 2)^2

checkCubedSum :: Int -> Bool
checkCubedSum n = cubedSum n == cubedSumSimple n

-- quickCheckResult (\(Positive n) -> checkCubedSum n)

-- Exercise 2
-- This is hard to test, because this requires a lot of computation for large
-- powersets. This checks a mathimatical fact but does not proof it.

checkPowerSetLength :: Int -> Bool
checkPowerSetLength n = length (subsequences [1..n]) == 2^n

data PositiveSmall = PositiveSmall Int deriving Show
instance Arbitrary PositiveSmall where
    arbitrary = fmap PositiveSmall (choose (1, 20))

-- quickCheckResult (\(PositiveSmall n) -> checkPowerSetLength n)

-- Exercise 3
-- See exercise 2.

factorial :: Int -> Int
factorial n = product [1..n]

data PositiveSmaller = PositiveSmaller Int deriving Show
instance Arbitrary PositiveSmaller where
    arbitrary = fmap PositiveSmaller (choose (1, 10))

-- quickCheckResult (\(PositiveSmaller n) -> length (permutations [1..n]) == factorial n)

-- Exercise 4
-- Reversal fails for negative numbers and numbers ending in zeros, but
-- there are no primes with this property so it poses no problem for
-- finding primes which reversed are primes as well.

reversal :: Int -> Int
reversal = read . reverse. show

checkReversal :: Int -> Bool
checkReversal n = reversal n == reversal (reversal n)

-- quickCheck checkReversal

findReversalPrimes :: Int -> [Int]
findReversalPrimes n = map reversal (filter prime (map reversal (takeWhile (<n) primes)))

-- Exercise 5
-- This does not need to be tested since the function continues until it
-- finds a correct answer.

consecutivePrimes :: Int -> Int -> [Int]
consecutivePrimes start n = take n (drop start primes)

findConsecutivePrimesPrimeSum :: Int -> Int
findConsecutivePrimesPrimeSum n = findConsecutivePrimesPrimeSum' 0 n

findConsecutivePrimesPrimeSum' :: Int -> Int -> Int
findConsecutivePrimesPrimeSum' start n
  | prime $ sum(consecutivePrimes start n) = sum(consecutivePrimes start n)
  | otherwise = findConsecutivePrimesPrimeSum' (start + 1) n

-- Exercise 6
-- The smallest counter example are the first 6 consecutive prime numbers.

consecutivePrimesProductPlusOne :: Int -> Int
consecutivePrimesProductPlusOne n = product (take n primes) + 1

-- quickCheckResult (\(NonNegative n) -> prime (consecutivePrimesProductPlusOne n))

-- Exercise 7 TODO make test data generator

luhn :: Int -> Bool
luhn accountNumber = mod digitSum 10 == 0
  where digitSum = luhnOdd (reverse (toDigits accountNumber)) 0

luhnOdd :: [Int] -> Int -> Int
luhnOdd [] digitSum = digitSum
luhnOdd digits digitSum = luhnEven (tail digits) (head digits + digitSum)

luhnEven :: [Int] -> Int -> Int
luhnEven [] digitSum = digitSum
luhnEven digits digitSum = luhnOdd (tail digits) (sum (toDigits (head digits * 2)) + digitSum)

isAmericanExpress, isMaster, isVisa :: Int -> Bool
isAmericanExpress accountNumber = start && len && (luhn accountNumber)
  where
    accountNumberDigits = toDigits accountNumber
    firstDigit = head accountNumberDigits
    secondDigit = head (drop 1 accountNumberDigits)
    start = firstDigit == 3 && (secondDigit == 4 || secondDigit == 7)
    len = length accountNumberDigits == 15

-- TODO
isMaster accountNumber = start && len && (luhn accountNumber)
  where
    accountNumberDigits = toDigits accountNumber
    start = True
    len = length accountNumberDigits == 16

isVisa accountNumber = start && len && (luhn accountNumber)
  where
    accountNumberDigits = toDigits accountNumber
    start = head(accountNumberDigits) == 4
    len = length accountNumberDigits == 13 || length accountNumberDigits == 16 || length accountNumberDigits == 19

checkDigit :: Int -> Int
checkDigit digitSum = head (reverse (toDigits (digitSum * 9)))

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

-- Bonus Problem 9

pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = [(a, b, c) | c <- [1..(div n 2)], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 && a + b + c == n]

-- Bonus Problem 10

primesSum :: Int -> Int
primesSum n = sum (takeWhile (<n) primes)
