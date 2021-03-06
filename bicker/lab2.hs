-- Student name:    Constantijn Bicker Caarten
-- Student number:  10427910

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

-- Exercise 1 (15 minutes)

-- filter_interval :: [Float] -> Float -> Float
filter_interval list a b = filter (>=a) (filter (<b) list)

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

quartiles_dist list = do
    probsList <- list
    return [length (filter_interval probsList 0.00 0.25),
            length (filter_interval probsList 0.25 0.50),
            length (filter_interval probsList 0.50 0.75),
            length (filter_interval probsList 0.75 1.00)]

list_values_offset x y h = h > x - x * y && h < x + x * y
-- prop_list_values_offset list x y = map (list_values_offset x y) list

-- prop_quartiles :: [Int] -> Int -> Float -> Bool
prop_list_values_offset [] _ _ = True
prop_list_values_offset (h:t) x y
    | h > x - x * y && h < x + x * y = prop_list_values_offset t x y
    | otherwise = False

-- quickCheckResult (\(Positive n) -> prop_list_values_offset (quartiles_dist (probs n)) (div n 4) 0.01)

-- Exercise 2 (15 minutes)

data Shape = NoTriangle | Equilateral | Isosceles
    | Rectangular | Other deriving (Eq,Show)

triangle :: Int -> Int -> Int -> Shape
triangle a b c
    | a + b < c || a + c < b || b + c < a = NoTriangle
    | a == b && a == c = Equilateral
    | a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2 = Rectangular
    | a == b || a == c || b == c = Isosceles
    | otherwise = Other

-- quickCheckResult (\ (Positive a) (Positive b) -> triangle a b (a + b + 1) == NoTriangle)
-- quickCheckResult (\ (Positive n) -> triangle n n n == Equilateral)
-- quickCheckResult (\ (Positive a) (Positive b) -> triangle a b (a^2 + b^2) == Rectangular) TODO a = 0
-- quickCheckResult (\ (Positive a) -> triangle a a (a + a) == Isosceles)
--

-- Exercise 3 (5 minutes)
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

workshop3a, workshop3b, workshop3c :: Int -> Bool
workshop3a x = even x && x > 3
workshop3b x = even x || x > 3
workshop3c x = (even x && x > 3) || even x

-- stronger [-10..10] workshop3a workshop3b

-- Exercise 4 (15 minutes)

isPermutation, isPermutation' :: Eq a => [a] -> [a] -> Bool
isPermutation listA listB
    | length listA == length listB = isPermutation' listA listB
    | otherwise = False

isPermutation' [] list = True
isPermutation' (h:t) list
    | elem h list = isPermutation' t (delete h list)
    | otherwise = False

-- Properties:
-- > Same length
-- > Same elements

data RandomIntListSmall = RandomIntListSmall [Int] deriving Show
instance Arbitrary RandomIntListSmall where
    arbitrary = fmap RandomIntListSmall (sublistOf [1..10])

-- quickCheckResult (\ (RandomIntListSmall xs) -> and (map (isPermutation xs) (permutations xs)))

-- Exercise 5 (7 minutes)

isDerangement, isDerangement' :: [Int] -> [Int] -> Bool
isDerangement listA listB
    | isPermutation listA listB = isDerangement' listA listB
    | otherwise = False

isDerangement' [] [] = True
isDerangement' (h1:t1) (h2:t2)
    | h1 == h2 = False
    | otherwise = isDerangement' t1 t2

deran :: [Int] -> [[Int]]
deran list = filter (isDerangement list) (permutations list)

-- Properties:
-- > Same length
-- > Same elements
-- > Different indices
-- > Self-inverses

-- Exercise 6 (35 minutes)

rot13 :: String -> String
rot13 w = rot13' w []

rot13' :: String -> String -> String
rot13' "" w2 = reverse w2
rot13' (h:t) w2
    | isLower h = rot13' t ((chr (mod (ord h - 97 + 13) 26 + 97)):w2)
    | isUpper h = rot13' t ((chr (mod (ord h - 65 + 13) 26 + 65)):w2)
    | otherwise = rot13' t (h:w2)

-- Specification:

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

-- quickCheckResult (\ (RandomString xs) -> prop_selfInverse xs)
-- quickCheckResult (\ (RandomString xs) -> prop_affect xs)
-- quickCheckResult (\ (RandomString xs) -> prop_nonAffect xs)

-- Exercise 7 (30 minutes)

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
-- and invalid IBANs. However this would need to be tested as well which
-- would be done using a validator similar to what needs to be tested in the
-- first place. This results circular reasoning so it is probably better
-- to use known test sets like shown above but much larger.
