--ROT13 is the transformation on a String that shifts specific letters by 13 characters and if done twice, results in the original String.

import Data.Char
import Test.QuickCheck

--valid_ascii_prop x = n >= 0 && n <= 255 where n = ord x
--input_prop xs = all (\x -> ord valid_ascii_prop x) xs 

alterable_letter_prop :: Char -> Bool
alterable_letter_prop x = (n >= 65 && n <= 90) || (n >= 97 && n <= 122) where n = ord x

same_length_prop :: [Char] -> [Char] -> Bool
same_length_prop xs ys = length xs == length ys

only_modified_alpha_letters :: [Char] -> [Char] -> Bool
only_modified_alpha_letters xs ys = all (\(letter,pos) ->
    ((not (alterable_letter_prop letter)) && (ys !! pos) == letter) || alterable_letter_prop letter) (zip xs [0..]) 

--this prop uses shift_letter, which is perhaps not a great way to specify things?
modified_letters_shifted_prop :: [Char] -> [Char] -> Bool
modified_letters_shifted_prop xs ys = all (\(letter,pos) ->
    (alterable_letter_prop letter &&(ys !! pos) == shift_letter letter) || (not (alterable_letter_prop letter))) (zip xs [0..])

output_prop :: [Char] -> [Char] -> Bool
output_prop xs ys = same_length_prop xs ys && only_modified_alpha_letters xs ys && modified_letters_shifted_prop xs ys

mapIf pred f = map (\x -> if pred x then f x else x)
shift_letter x | n > (122 - 13) && n <= 122 = chr (n + 13 - 26) 
               | n > (90 - 13) && n <= 90 = chr (n + 13 - 26)
               | otherwise = chr (n + 13)
               where n = ord x
rot xs = mapIf alterable_letter_prop shift_letter xs


data RandomString = RandomString String deriving Show
instance Arbitrary RandomString where
    arbitrary = fmap RandomString (listOf $ elements (map chr [0..127]))

main = do
    quickCheckResult (\(RandomString s) -> output_prop s (rot s))
    quickCheckResult (\(RandomString s) -> rot (rot s) == s)

-- time taken: 90m
