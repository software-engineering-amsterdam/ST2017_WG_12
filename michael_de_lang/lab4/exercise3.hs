import Control.Monad
import SetOrd
import Test.QuickCheck
import Test.QuickCheck.Monadic

getValuesFromSet :: Set Int -> [Int]
getValuesFromSet (Set a) = a

setLength :: Set Int -> Int
setLength (Set xs) = length xs

generateSetQuickCheck :: Int -> Int -> Int -> Gen (Set Int)
generateSetQuickCheck n lower upper = do
    xs <- vectorOf n $ choose (lower, upper)
    return $ list2set xs

setIntersection :: Set Int -> Set Int -> Set Int
setIntersection a b = do
    let as = getValuesFromSet a
    let bs = getValuesFromSet b
    list2set $ filter (\x -> elem x bs) as

setUnion :: Set Int -> Set Int -> Set Int
setUnion a b = do
    let as = getValuesFromSet a
    let bs = getValuesFromSet b
    list2set $ (as ++ bs)

setDifference :: Set Int -> Set Int -> Set Int
setDifference a b = do
    let as = getValuesFromSet a
    let bs = getValuesFromSet b
    list2set $ filter (\x -> notElem x as || notElem x bs) (as ++ bs)

prop_sameInput :: Property
prop_sameInput = monadicIO $ do
    set <- run $ generate $ generateSetQuickCheck 10 1 100
    assert $ (setIntersection set set) == set 
    assert $ (setUnion set set) == set
    assert $ (setDifference set set) == Set []
    
prop_length :: Property
prop_length = monadicIO $ do
    set1 <- run $ generate $ generateSetQuickCheck 10 1 100
    set2 <- run $ generate $ generateSetQuickCheck 10 1 100
    assert $ setLength (setIntersection set1 set2) <= min (setLength set1) (setLength set2)
    assert $ setLength (setUnion set1 set2) >= max (setLength set1) (setLength set2)
    assert $ setLength (setDifference set1 set1) <= min (setLength set1) (setLength set2)
    
main = do
    quickCheck $ prop_sameInput
    quickCheck $ prop_length
    
    
-- time spent: 90m

-- output:
-- *Main> main
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.