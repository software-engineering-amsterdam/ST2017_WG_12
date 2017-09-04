--module Lab1 where
import Data.List
import Test.QuickCheck

data NonNegativeSmall = NonNegativeSmall Integer deriving Show
instance Arbitrary NonNegativeSmall where
    arbitrary = fmap NonNegativeSmall (choose (1, 25))

-- please compile with -O2
main = do quickCheckResult (\(NonNegativeSmall x) -> 2^(length [1..x]) == length (subsequences [1..x]))

-- Making the generator generate lists of size [1..25] means that there'll be redundant tests.
-- Making the generated lists larger than this leads to exponential times, after 25 my machine simply takes too long. This is due to powersets by definition creating exponential results. Permutating over that simply requires a lot of computational power.
-- What is actually tested here is if subsequences produces a list of lists with the expected size, but the actual contents(whether the list is unique for example) is not tested. 
-- This means that what we're actually testing is whether or not this function generates data that at least *looks* like it fits with our hypothesis.

-- time taken for writing this exercise: 60 minutes (of which ~20 minutes spend on figuring out custom modifiers)
