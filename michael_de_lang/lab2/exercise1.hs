import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

data NonNegativeLarge = NonNegativeLarge Int deriving Show
instance Arbitrary NonNegativeLarge where
    arbitrary = fmap NonNegativeLarge (choose (5000, 50000))

getQuartile :: Int -> [Float] -> [Float]
getQuartile 0 prob = filter (< 0.25) prob
getQuartile 1 prob = filter (\x -> x >= 0.25 && x < 0.5) prob
getQuartile 2 prob = filter (\x -> x >= 0.5 && x < 0.75) prob
getQuartile 3 prob = filter (\x -> x >= 0.75 && x < 1) prob

checkQuartiles :: [Float] -> Int -> Bool
checkQuartiles prob n = all (\x -> abs (length (x) - (div n 4)) < div n 20) [(getQuartile 0 prob), (getQuartile 1 prob), (getQuartile 2 prob), (getQuartile 3 prob)]

check :: Int -> Property
check x = monadicIO $ do
    prob <- run (probs x)
    assert (checkQuartiles prob x)

main = quickCheckResult (\(NonNegativeLarge x) -> check x)


-- time for creating haskell code: 90m
-- time for figuring out what the margin of error was: 60m

-- The result of this test is 100 successful runs of randomly sized sets of random numbers. This means that the generator is uniform enough for the given margin of error (5% in this case)
-- Therefore, this RNG is usable in situations PRNGs are employed.

-- I interpretted "roughly has to be around 2500" to mean a margin of error of 5%, in which case using multiple runs doesn't really contribute anything over just increasing the amount of randomly generated numbers in 1 test run.
-- However, in the case that you'd actually want to calculate the margin of error for a specific confidence level and check the generator against that,
-- you'd have to do multiple runs, calculate the mean of each set to get a normal distribution and then check that distribution against the margin of error.
-- At least, that's what I got from a cursory glance at statistical analysis.
