import Data.List
import Test.QuickCheck

data NonNegativeSmall = NonNegativeSmall Int deriving Show
instance Arbitrary NonNegativeSmall where
    arbitrary = fmap NonNegativeSmall (choose (0, 11))

factorial :: [Int] -> Int
factorial [] = 1
factorial(x:xs) = x * factorial xs
main = quickCheckResult (\(NonNegativeSmall n) -> (length (permutations [1..n])) == (factorial [1..n]))
--time taken: ~120 minutes
