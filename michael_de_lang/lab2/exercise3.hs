import Data.List

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

prop_one, prop_two, prop_three, prop_four :: Int -> Bool

--(\ x -> even x && x > 3)
prop_one x = even x && x > 3

--(\ x -> even x || x > 3)
prop_two x = even x || x > 3

--(\ x -> (even x && x > 3) || even x
prop_three x = (even x && x > 3) || even x

--even
prop_four x = mod x 2 == 0


bubbleSort' :: [(Int, (Int -> Bool))] -> [(Int, (Int -> Bool))]
bubbleSort' [] = []
bubbleSort' ((n,x):[]) = [(n,x)]
bubbleSort' ((n,x):(n',y):xs) | stronger [(-10)..10] x y = (n,x) : bubbleSort' ((n',y):xs) 
                              | otherwise = bubbleSort' [(n',y),(n,x)] ++ xs

-- this last part gotten from https://smthngsmwhr.wordpress.com/2012/11/09/sorting-algorithms-in-haskell/#bubble_sort
bubbleSort ::  [(Int, (Int -> Bool))] -> Int -> [(Int, (Int -> Bool))]
bubbleSort xs i | i == length xs = xs
                | otherwise = bubbleSort (bubbleSort' xs) (i + 1)

main = putStrLn $ show $ map (\(x,y) -> x) (bubbleSort [(1, prop_one), (2, prop_two), (3, prop_three), (4, prop_four)] 0)
--time so far: 30m
