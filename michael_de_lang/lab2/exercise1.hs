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
checkQuartiles prob n = all (\x -> abs (length (x) - (div n 4)) < div n 1000) [(getQuartile 0 prob)]

check :: Int -> Property
check x = monadicIO $ do
    prob <- run (probs x)
    assert (checkQuartiles prob x)

main = quickCheckResult (\(NonNegativeLarge x) -> check x)


-- time so far: 90m
