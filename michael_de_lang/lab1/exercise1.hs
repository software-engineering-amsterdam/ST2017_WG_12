module Lab1 where
import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
    where xs = takeWhile(\y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> (Bool)
forall = flip all

-- 1.
sumExerciseTwo :: Integer -> Integer
sumExerciseTwo x | x < 0 = error "negative number" 
sumExerciseTwo 0 = 0
sumExerciseTwo x = sumExerciseTwo (x-1) + x^2
sumExerciseTwo' :: Integer -> Integer
sumExerciseTwo' x | x < 0 = error "negative number"
sumExerciseTwo' x = (x*(x+1)*(2*x+1)) `div` 6
-- *Lab1> quickCheckResult (\(NonNegative x) -> sumExerciseTwo x == sumExerciseTwo' x)
-- +++ OK, passed 100 tests.
-- Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}

sumExerciseThree :: Integer -> Integer
sumExerciseThree x | x < 0 = error "negative number"
sumExerciseThree 0 = 0
sumExerciseThree x = sumExerciseThree (x-1) + x^3
sumExerciseThree' :: Integer -> Integer
sumExerciseThree' x | x < 0 = error "negative number"
sumExerciseThree' x = (x*(x+1) `div` 2)^2  
-- *Lab1> quickCheckResult (\(NonNegative x) -> sumExerciseThree x == sumExerciseThree' x)
-- +++ OK, passed 100 tests.
-- Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}

