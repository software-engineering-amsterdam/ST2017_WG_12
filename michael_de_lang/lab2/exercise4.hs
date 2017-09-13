import Data.List
import Test.QuickCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = length a == length b && all (\x -> length x == length (filter (== (x !! 0)) b)) (group a)

equalLength_Prop :: Eq a => [a] -> [a] -> Bool
equalLength_Prop a b = length a == length b

groupedElementsCount_Prop :: Eq a => [a] -> [a] -> Bool
groupedElementsCount_Prop a b = all (\x -> length x == length (filter (== (x !! 0)) b)) (group a)

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

sortProp (_,f) (_,f') | stronger [(-10)..10] f f' = LT
                      | weaker [(-10)..10] f f' = GT
                      | otherwise = EQ


main = do
    let permutations = [[1,2,3], [1,2,3], [1,3,2], [3,1,2], [3,2,1], [2,1,3], [2,3,1]]
    let notPermutations = [[1,2,3], [1,2,2], [], [1], [3,2,2]]
    let duplicates = [[1,2,2,3], [2,1,2,3], [3,2,2,1], [3,1,2,2]]

    putStrLn $ show $ map (\(x,_) -> x) $ sortBy sortProp [(1,equalLength_Prop), (2,groupedElementsCount_Prop)]

    if all (\x -> isPermutation (permutations !! 0) x) (drop 1 permutations) then
        putStrLn "Passed"
    else
        putStrLn "Failed"

    if all (\x -> not (isPermutation (notPermutations !! 0) x)) (drop 1 notPermutations) then
        putStrLn "Passed"
    else
        putStrLn "Failed"

    if all (\x -> isPermutation (duplicates !! 0) x) (drop 1 duplicates) then
        putStrLn "Passed"
    else
        putStrLn "Failed"


-- time taken so far: 30m
