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
prop_four x = mod x 2 ==  0

sortProp (_,f) (_,f') | stronger [(-10)..10] f f' = LT
                      | weaker [(-10)..10] f f' = GT
                      | otherwise = EQ

main = putStrLn $ show $ map (\(x,y) -> x) (sortBy sortProp [(1, prop_one), (2, prop_two), (3, prop_three), (4, prop_four)])
--time so far: 30m
