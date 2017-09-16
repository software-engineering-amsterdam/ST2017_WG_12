-- THIS IS NOT THE FINAL VERSION
-- THOU SHALL NOT PASS WITH THIS
-- Niels Boerkamp
-- Michael de Lang
-- Constantijn Bicker Caarten
-- Arjan Meijer

-- Lab 2 exercises
-- 17-09-2017
module Lab2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Debug.Trace


data NonNegativeLarge = NonNegativeLarge Int deriving Show
instance Arbitrary NonNegativeLarge where
    arbitrary = fmap NonNegativeLarge (choose (5000, 50000))

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

upNth :: Int -> [Int] -> [Int]
upNth n []     | n == 0 = [1]
               | otherwise = 0:upNth (n-1) []
upNth n (x:xs) | n == 0 = (x + 1):xs
               | otherwise = x:upNth (n-1) xs

-- Exercise 1
-- We used the Exercise of Arjan because he had most efficient way of correctly determining the values
-- We used the Test of Michael because he had the cleanest way of testing the code
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

checkProbs :: [Int] -> [Float] -> [Int]
checkProbs count (x:xs) | x > 0.75 = checkProbs (upNth 3 count) xs
                        | x > 0.5 = checkProbs (upNth 2 count) xs
                        | x > 0.25 = checkProbs (upNth 1 count) xs
                        | otherwise = checkProbs (upNth 0 count) xs
checkProbs count [] = count

checkBalance :: [Int] -> Int -> Bool
checkBalance xs y = (maximum xs) - (minimum xs) < div y 10

check :: Int -> Property
check x = monadicIO $ do
    prob <- run (probs x)
    assert (checkBalance (checkProbs [0,0,0,0] prob) x)

exerciseOne = quickCheckResult (\(NonNegativeLarge x) -> check x)

-- Exercise 1 manual test results:
--        [2505,  2510, 2482, 2503]
--        [2458,  2476, 2488, 2578]
--        [2536,  2517, 2508, 2439]
--        [2560,  2478, 2440, 2522]
-- TOTAL: [10059, 9981, 9918, 10042]
-- There are no big differences over the sum of four test,
-- But in this test set there is a bigger chance that a number 
-- falls in the first or the last quartile

-- Resulst of a test with more numbers:
--        [25117,  24975,  24922, 24986]
--        [24760,  25129,  25067, 25044]
--        [25288,  24947,  24933, 24832]
--        [24856,  25080,  24961, 25103]
-- TOTAL: [100021, 100131, 99883, 99965]
-- There are also no big defferences in the sum of these four tests,
-- Therefore we can assume that the method works

-- Exercise 2
-- We used the Exercise of Niels because he used property functions to check the properties 
-- of a triangle
-- We used the Tests of Constantijn because he used quickCheck for the test
-- We used the limited tests of Michael because he created the tests for the exceptions

data Shape = NoTriangle | Equilateral | Isosceles
    | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | prop_no_triangle x y z = NoTriangle 
               | prop_equilateral x y z = Equilateral
               | prop_rectangular x y z = Rectangular
               | prop_isosceles x y z = Isosceles
               | otherwise = Other

prop_rectangular, prop_no_triangle, prop_equilateral :: Integer -> Integer -> Integer -> Bool
prop_rectangular x y z = (x^2 + y^2 == z^2) || (z^2 + y^2 == x^2) || (z^2 + x^2 == y^2)
prop_no_triangle x y z = (x + y < z) || (x + z < y) || (y + z < x)
prop_equilateral x y z = x == y && y == z
prop_isosceles x y z = x == y || y == z || x == z

exerciseTwo = do
                quickCheckResult (\ (Positive a) (Positive b) (Positive c) -> triangle a b (a + b + c) == NoTriangle)
                quickCheckResult (\ (Positive n) -> triangle n n n == Equilateral)
                quickCheckResult (\ (Positive a) -> triangle a a (a + a) == Isosceles)
                let rectangulars = [(3,4,5), (3,5,4), (4,5,3), (5,13,12), (5,12,13)]
                let others = [(2,3,4), (3,2,4), (4,3,2)] 
                putStrLn $ show $ all (\(a, b, c) -> triangle a b c == Rectangular) rectangulars
                putStrLn $ show $ all (\(a, b, c) -> triangle a b c == Other) others
                
-- Rectangular and Other are not possible with quickCheck, these triangles have properties that cannot
-- be checked with random numbers

-- Exercise 3
-- We used Arjans exercise 3a, because he displayed a list of results
-- We used Michaels exercise 3b, because he used the least amount of code to implement it

-- A
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs p q = stronger xs q p

-- Workshop 3 exercises
e1, e2, e3, e4 :: Int -> Bool
e1 x = even x && x > 3
e2 x = even x || x > 3
e3 x = (even x && x > 3) || even x
e4 x = (even x && x > 3) || even x

exerciseThreeA = do
                 print $ "The first is " ++ if stronger [(-10)..10] e1 even then "stronger" else "weaker"
                 print $ "The first is " ++ if stronger [(-10)..10] e2 even then "stronger" else "weaker"
                 print $ "The first is " ++ if stronger [(-10)..10] e3 even then "stronger" else "weaker"
                 print $ "The first is " ++ if stronger [(-10)..10] e4 even then "stronger" else "weaker"

-- B
sortProp (_,f) (_,f') | stronger [(-10)..10] f f' = LT
                      | weaker [(-10)..10] f f' = GT
                      | otherwise = EQ

exerciseThreeB = putStrLn $ show $ map (\(x,y) -> x) (sortBy sortProp [(1, e1), (2, e2), (3, e3), (4, e4)])

-- Exercise 4
-- We used the implementation of Constantijn for this exercise because he had the most efficient code
-- We used the test properties of Niels because he had the cleanest properties
isPermutation, isPermutation' :: Eq a => [a] -> [a] -> Bool
isPermutation listA listB
    | length listA == length listB = isPermutation' listA listB
    | otherwise = False

isPermutation' [] list = True
isPermutation' (h:t) list
    | elem h list = isPermutation' t (delete h list)
    | otherwise = False

-- Test cases
permTest1 = isPermutation [1..100] [1..100] -- should pass -> Passes
permTest2 = isPermutation [1,2,3,4] [1,2,4,3] -- should pass -> Passes
permTest3 = isPermutation ([]::[Int]) ([]::[Int]) -- should pass -> Passes
permTest4 = isPermutation [1] [] -- should fail -> Fails

-- Test properties
sameLength :: Eq a => [a] -> [a] -> Bool
sameLength xs ys = length (group xs) == length (group ys)

data RandomIntListSmall = RandomIntListSmall [Int] deriving Show
instance Arbitrary RandomIntListSmall where
    arbitrary = fmap RandomIntListSmall (sublistOf [1..10])

exerciseFour = do 
                 print $ show $ permTest1 && permTest2 && permTest3 && not permTest4
                 quickCheckResult (\ (RandomIntListSmall xs) -> (and (map (isPermutation xs) (permutations xs))))

-- Exercise 5
-- We used the implementation of Niels because he had the most compact code
-- We used the test of Michael because he created the best tests
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && and (zipWith (/=) xs ys)

deran :: Eq a => [a] -> [[a]]
deran xs = filter (isDerangement xs) (permutations xs)

equalLength_Prop :: Eq a => [a] -> [a] -> Bool
equalLength_Prop a b = length a == length b

groupedElementsCount_Prop :: Eq a => [a] -> [a] -> Bool
groupedElementsCount_Prop a b = all (\x -> length x == length (filter (== (x !! 0)) b)) (group a)

positionsDiffer_Prop :: Eq a => [a] -> [a] -> Bool
positionsDiffer_Prop a b = all (\x -> (a !! x) /= (b !! x)) [0..((length a)-1)]

exerciseFive = do
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

-- Our answer would be that while technically possible, you shouldn't automate this test and instead use known values to test the function.

-- Exercise 6
-- We used the exercise of Constantijn because the exercises were very simular but he created
-- The best tests
rot13 :: String -> String
rot13 w = rot13' w []

rot13' :: String -> String -> String
rot13' "" w2 = reverse w2
rot13' (h:t) w2
    | isLower h = rot13' t ((chr (mod (ord h - 97 + 13) 26 + 97)):w2)
    | isUpper h = rot13' t ((chr (mod (ord h - 65 + 13) 26 + 65)):w2)
    | otherwise = rot13' t (h:w2)

data RandomString = RandomString String deriving Show
instance Arbitrary RandomString where
    arbitrary = fmap RandomString (listOf $ elements (map chr [0..127]))

prop_selfInverse, prop_affect, prop_nonAffect :: String -> Bool
prop_selfInverse string = string == rot13 (rot13 string)

prop_affect string
    | string' == "" = True
    | otherwise = string' /= rot13 string'
    where string' = filter (\ x -> elem x (['a'..'z'] ++ ['A'..'Z'])) string

prop_nonAffect string = string' == rot13 string'
    where string' = filter (\ x -> not (elem x (['a'..'z'] ++ ['A'..'Z']))) string

exerciseSix = do
               quickCheckResult (\ (RandomString xs) -> prop_selfInverse xs)
               quickCheckResult (\ (RandomString xs) -> prop_affect xs)
               quickCheckResult (\ (RandomString xs) -> prop_nonAffect xs)

-- Exercise 7
-- We used the exercise of Constantijn because he had the cleanest code and supported the most countries
ibanValidation :: String -> Bool
ibanValidation iban = ibanCountryValidation iban && (mod (ibanReplaceLetters iban) 97 == 1)

ibanCountryValidation :: String -> Bool
ibanCountryValidation iban
    | cc == "BE" = length iban == 16 -- Belgium
    | cc == "BR" = length iban == 29 -- Brazil
    | cc == "CH" = length iban == 21 -- Switzerland
    | cc == "DE" = length iban == 22 -- Germany
    | cc == "ES" = length iban == 24 -- Spain
    | cc == "FR" = length iban == 27 -- France
    | cc == "GB" = length iban == 22 -- United Kingdom
    | cc == "GR" = length iban == 27 -- Greece
    | cc == "IE" = length iban == 22 -- Ireland
    | cc == "IT" = length iban == 27 -- Italy
    | cc == "NL" = length iban == 18 -- Netherlands
    | cc == "NO" = length iban == 15 -- Norway
    | cc == "PL" = length iban == 28 -- Poland
    | cc == "PT" = length iban == 25 -- Portugal
    | cc == "TR" = length iban == 26 -- Turkey
    | otherwise = False
    where cc = take 2 iban

ibanReplaceLetters :: String -> Integer
ibanReplaceLetters iban = joiner (map ibanCharToInt (ibanRearange iban))

ibanRearange :: String -> String
ibanRearange iban = (drop 4 iban) ++ (take 4 iban)

ibanCharToInt :: Char -> Integer
ibanCharToInt char
    | isLower char = toInteger (ord char - 87)
    | isUpper char = toInteger (ord char - 55)
    | otherwise = toInteger (digitToInt char)

-- source: https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell/1918522#1918522
joiner :: [Integer] -> Integer
joiner = read . concatMap show

ibanTest :: Bool
ibanTest = and (map ibanValidation testSetTrue) && and (map not (map ibanValidation testSetFalse))
    where
        testSetTrue = ["BE62510007547061","FR1420041010050500013M02606","DE89370400440532013000","GR1601101250000000012300695","IE29AIBK93115212345678","IT40S0542811101000000123456","NL39RABO0300065264","NO9386011117947","PL60102010260000042270201111","PT50000201231234567890154","ES8023100001180000012345","CH9300762011623852957","TR330006100519786457841326"]
        testSetFalse = ["BE6251000754706", "BE62510007547062", "BX62510007547062", "IT40S0542811101000000123459", "ZT40S0542811101000000123456"]

-- It is possible to automate the test process by generating random valid
-- and invalid IBANs. Generating valid IBANs can be done by using the
-- requierements...