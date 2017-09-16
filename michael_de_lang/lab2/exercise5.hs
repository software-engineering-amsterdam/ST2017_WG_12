import Data.List
import Test.QuickCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = length a == length b && all (\x -> length x == length (filter (== (x !! 0)) b)) (group a)

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement a b = isPermutation a b && all (\x -> (a !! x) /= (b !! x)) [0..((length a)-1)]

genDerangements :: Eq a => [a] -> [[a]]
genDerangements a = filter (\x -> isDerangement x a) (permutations a)

-- props

equalLength_Prop :: Eq a => [a] -> [a] -> Bool
equalLength_Prop a b = length a == length b

groupedElementsCount_Prop :: Eq a => [a] -> [a] -> Bool
groupedElementsCount_Prop a b = all (\x -> length x == length (filter (== (x !! 0)) b)) (group a)

positionsDiffer_Prop :: Eq a => [a] -> [a] -> Bool
positionsDiffer_Prop a b = all (\x -> (a !! x) /= (b !! x)) [0..((length a)-1)]


infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

stronger, weaker :: Num a => [([a],[a])] -> ([a] -> [a] -> Bool) -> ([a] -> [a] -> Bool) -> Bool
stronger xs p q = forall xs (\(x,y) -> p x y --> q x y)
weaker   xs p q = stronger xs q p

sortPropPerms = [([1,2,3],[3,2,1]),([1,2,2,3],[1,2,2,3])]

sortProp (_,f) (_,f') | stronger [([1,2,3],[3,2,1])] f f' = LT
                      | weaker [([1,2,3],[3,2,1])] f f' = GT
                      | otherwise = EQ


main = do
    putStrLn $ show $ map (\(x,_) -> x) $ sortBy sortProp [(1,equalLength_Prop), (2,groupedElementsCount_Prop), (3,positionsDiffer_Prop)]
    let correctDerangements = [[1,2,3,4],[4,3,2,1],[3,4,2,1],[2,3,4,1],[4,1,2,3],[2,4,1,3],[2,1,4,3],[4,3,1,2],[3,4,1,2],[3,1,4,2]]
    let incorrectDerangements = [[1,2,3,4],[1,2,3,4],[1,4,3,2],[4,2,1,3]]

    if all (\x -> isDerangement x (correctDerangements !! 0)) (drop 1 correctDerangements) then
        putStrLn "Passed"
    else
        putStrLn "Failed"

    if not (all (\x -> isDerangement x (incorrectDerangements !! 0)) (drop 1 incorrectDerangements)) then
        putStrLn "Passed"
    else
        putStrLn "Failed"
-- You could automate this by generating random lists and using genDerangements to generate all derangements and checking if it's correct.
-- The problem here though is that you're using the function under test to generate input for said function, making me wonder what you would actually be testing.
-- Incorrect derangements can be generated with the permutations function.

-- My answer would be that while technically possible, you shouldn't automate this test and instead use known values to test the function.
--time taken: 150m
