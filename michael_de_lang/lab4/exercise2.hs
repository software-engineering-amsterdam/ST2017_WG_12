import Control.Monad
import System.Random
import SetOrd
import Test.QuickCheck
import Test.QuickCheck.Monadic

allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = notElem x xs && allUnique xs

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (x:[]) = True
isSorted (x:y:xs) = x < y && isSorted (y:xs)

getValuesFromSet :: Set Int -> [Int]
getValuesFromSet (Set a) = a

generateList n lower upper = replicateM n $ randomRIO (lower,upper :: Int)

turnListIntoSet :: IO [Int] -> IO (Set Int)
turnListIntoSet xs = do
    xs' <- xs
    return $ list2set xs'

generateSetQuickCheck :: Int -> Int -> Int -> Gen (Set Int)
generateSetQuickCheck n lower upper = do
    xs <- vectorOf n $ choose (lower, upper)
    return $ list2set xs

prop_noDuplicates :: Gen (Set Int) -> Property
prop_noDuplicates gen = monadicIO $ do
    set <- run $ generate gen
    assert $ allUnique $ getValuesFromSet set

prop_orderedSet :: Gen (Set Int) -> Property
prop_orderedSet gen = monadicIO $ do
    set <- run $ generate gen
    assert $ isSorted $ getValuesFromSet set

main = do
    genSet <- turnListIntoSet $ generateList 10 1 100
    print genSet
    quickCheck $ prop_noDuplicates $ generateSetQuickCheck 10 1 100
    quickCheck $ prop_orderedSet $ generateSetQuickCheck 10 1 100

-- time taken: 150m
