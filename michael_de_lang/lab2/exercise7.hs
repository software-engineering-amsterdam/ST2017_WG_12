import Data.List
import Data.Char
import Test.QuickCheck

ibanLength :: String -> Int
ibanLength ('A':'L':xs) = 28

letterToNum :: Char -> Int
letterToNum x | n >= 48 && n <= 57 = n - 48
              | otherwise = n - 55
              where n = ord x

moveCountryCode :: String -> String
moveCountryCode xs = drop 4 xs ++ take 4 xs

checkNum :: [Int] -> Int
checkNum = foldl (\n d -> n*10 + d) 0 

computeCheckDigit :: Int -> Int
computeCheckDigit x = 98 - (mod x 97)

iban :: String -> Bool
iban xs = length xs == checkLength && checkDigits  == computedCheckDigits
    where checkLength = ibanLength (take 2 xs)
          checkNum = foldl (\n d -> n*10 + d) 0 (map letterToNum (moveCountryCode xs))
          computedCheckDigits = 98 - (mod checkNum 97)
          checkDigits = (letterToNum (xs !! 2))*10 + letterToNum (xs !! 3)


-- time taken: 30m
-- Didn't have enough time this week to finish all the exercises.
-- This exercise isn't correct, since checkNum is not a correct [Int] -> Int function.
-- If I had more time, I would've tested this by using lists of known correct and incorrect values.
-- Naturally, this testset would have to be rather large to give a high enough confidence that the iban function is implemented correctly.
-- Automating this test is possible by introducing input properties such as the property that the length of the input has to be equal to what is set for a certain country,
-- or the the mod-97-10 property.
-- Using these properties to generate input values that do or do not cohere to these properties should yield results from the iban function that can be known by the test.
