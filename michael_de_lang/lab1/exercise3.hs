import Data.List
import Test.QuickCheck

data NonNegativeSmall = NonNegativeSmall Int deriving Show
instance Arbitrary NonNegativeSmall where
    arbitrary = fmap NonNegativeSmall (choose (0, 11))

factorial :: [Int] -> Int
factorial [] = 1
factorial(x:xs) = x * factorial xs
main = quickCheckResult (\(NonNegativeSmall n) -> (length (permutations [1..n])) == (factorial [1..n]))

-- Just like the previous exercise, this property is hard to test due to factorial time requirements, as is evident from the factorial length comparison.
-- And likewise, just like the previous exercise, we're only testing a part of the specification and not checking whether pemutations generates proper data, just the proper length of an array.
--time taken: ~120 minutes
