import Data.List
import Test.QuickCheck

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


main = quickCheckResult (\(NonNegative x) -> sumExerciseThree x == sumExerciseThree' x)
-- +++ OK, passed 100 tests.
-- Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}

-- time taken for writing this exercise: 20 minutes
