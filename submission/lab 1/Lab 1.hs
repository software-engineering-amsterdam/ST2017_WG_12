-- Niels Boerkamp
-- Michael de Lang
-- Constantijn Bicker Caarten
-- Arjan Meijer

-- Lab 1 exercises
-- 05-09-2017

import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

primesBelow :: Integer -> [Integer]
primesBelow 0 = []
primesBelow x = takeWhile (< x) primes

data NonNegativeSmall = NonNegativeSmall Integer deriving Show
instance Arbitrary NonNegativeSmall where
    arbitrary = fmap NonNegativeSmall (choose (1, 10))

-- Exercise 1
-- We used the exercise of Niels Boerkamp because we thought that this one is
-- easy to read, as well as all our work looking quite alike and having no clear winner
-- on other metrics.

-- A
sumNumbersSquared :: Integer -> Integer
sumNumbersSquared x = sum( map (^2) [0..x])

exercise1a :: Integer -> Bool
exercise1a x = sumNumbersSquared x == (x * (x + 1) * (2 * x + 1)) `div` 6

-- B
sumNumbersCubed :: Integer -> Integer
sumNumbersCubed x = sum (map(^3) [0..x])

exercise1b :: Integer -> Bool
exercise1b x = sumNumbersCubed x == ((x * (x + 1)) `div` 2) ^ 2

-- Execution:
exerciseOne = do
    quickCheckResult (\(NonNegative n) -> exercise1a n)
    quickCheckResult (\(NonNegative n) -> exercise1b n)

-- Exercise 2
-- We used the exercise of Michael because we thought that this one is the easiest to read
-- there were no mayor differences between our versions

exerciseTwo = do quickCheckResult (\(NonNegativeSmall x) -> 2^(length [1..x]) == length (subsequences [1..x]))

-- Making the generator generate lists of size [1..25] means that there'll be redundant tests.
-- Making the generated lists larger than this leads to exponential times, after 25 my machine simply takes too long. This is due to powersets by definition creating exponential results. Permutating over that simply requires a lot of computational power.
-- What is actually tested here is if subsequences produces a list of lists with the expected size, but the actual contents(whether the list is unique for example) is not tested.
-- This means that what we're actually testing is whether or not this function generates data that at least *looks* like it fits with our hypothesis.

-- inspirations:
-- https://www.stuartgunter.org/posts/intro-to-quickcheck/
-- https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
-- https://stackoverflow.com/questions/13354726/using-a-custom-generator-vs-arbitrary-instance-in-quickcheck

-- Exercise 3
-- We used the exercise of Constantijn because we thought this exercise used the most efficient and compact calculation method for the factorial

-- This is hard to test, because this requires a lot of computation for large
-- powersets since its length grows exponential. This checks a mathimatical
-- fact but does not proof it.

factorial :: Int -> Int
factorial n = product [1..n]

-- Custom test data set with range [1-20]
data PositiveSmall = PositiveSmall Int deriving Show
instance Arbitrary PositiveSmall where
   arbitrary = fmap PositiveSmall (choose (1, 20))

exerciseThree = do quickCheckResult (\(PositiveSmall n) -> length (subsequences [1..n]) == 2^n)

-- Exercise 4

-- We used the exercise of Constantijn because he did not only test and create the findReversalPrimes, but he also tested the reversal method itself

-- Reversal check
-- Reversal fails for negative numbers and numbers ending in zeros, but
-- there are no primes with this property so this poses no problem for
-- finding primes which reversed are primes as well.

reversal :: Integer -> Integer
reversal = read . reverse. show

findReversalPrimes :: Integer -> [Integer]
findReversalPrimes n = map reversal (filter prime (map reversal (takeWhile (<n) primes)))

exerciseFour = do
    putStrLn $ show $ findReversalPrimes 10000;
    quickCheckResult(\(NonNegative n) -> n == reversal (reversal n))

-- Exercise 5
-- We used the exercise of Michael because he created a single function that solves this exercise,
-- the rest of the team splitted it in two or more functions.
sumSmallestConsecutivePrimes :: Int -> Int -> [Integer]
sumSmallestConsecutivePrimes x s | prime (sum consecutivePrimes) = (sum consecutivePrimes) : consecutivePrimes
                                 | otherwise = sumSmallestConsecutivePrimes x (s+1)
                                 where consecutivePrimes = (take x (drop s primes))

-- output is a list with 102 elements, the 0th element being the answer and every element after it are the primes used to sum to get to this answer.
exerciseFive = do
    putStrLn $ show $ sumSmallestConsecutivePrimes 101 0

-- The function itself does not need to be tested since it will find a answer that satisfies the given formula.
-- Although we can check the result of the funcion if we test it on a pre-determined data set.
-- An method could be:
-- I.   The given are primes and should be consecutive
-- II.  The sum must be correct
-- III. The sequence should start as low as possible


-- ORIGIONAL COMMENTS:
-- This does not need to be tested since the function continues until it
-- finds a correct answer.

-- A test would be to check sumSmallestConsecutivePrimes against known correct values. sumSmallestConsecutivePrimes 2, for example, should give 2 + 3 = 5 and sumSmallestConsecutivePrimes 3 should give 5 + 7 + 11 = 23

-- An answer should always be tested if that is possible
-- To test this answer we have to check three things,
-- I.   The given are primes and should be consecutive
-- II.  The sum must be correct
-- III. The sequence should start as low as possible
-- END ORIGIONAL COMMENT

-- Exercise 6
-- We used the exercise of Constantijn because he wrote the most compact code, there were no big differences in our answers
-- The smallest counter example are the first 6 consecutive prime numbers.

consecutivePrimesProductPlusOne :: Int -> Integer
consecutivePrimesProductPlusOne n = product (take n primes) + 1

exerciseSix = do
    quickCheckResult (\(NonNegative n) -> prime (consecutivePrimesProductPlusOne n))

-- Exercise 7
-- We used the exercise of Michael because he created the most comprehensice test set
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []       = []
altMap f g (x:[])   = f x : []
altMap f g (x:y:xs) = f x : g y : altMap f g xs

luhnDouble :: Integer -> Integer
luhnDouble x = if n < 10 then n else n - 9
               where n = x*2

mod10NoRemainder :: Integral a => a -> Bool
mod10NoRemainder x = (mod x 10) == 0

luhn :: [Integer] -> Bool
luhn = mod10NoRemainder . sum . (altMap id luhnDouble) . reverse

-- taken from https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
toDigits :: Integer -> [Integer]
toDigits n = map (\x -> read [x] :: Integer) (show n)

-- taken from https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
toInt :: [Integer] -> Integer
toInt = foldl addDigit 0 where addDigit num d = 10*num + d

isAmericanExpress :: Integer -> Bool
isAmericanExpress x = (firstTwo == 34 || firstTwo == 37) && length digits == 15 && luhn digits where
    digits = toDigits x
    firstTwo = toInt $ take 2 digits

isMaster :: Integer -> Bool
isMaster x = ((firstTwo >= 51 && firstTwo <= 55) || (firstFour >= 2221 && firstFour <=2720)) && length digits == 16 && luhn digits where
    digits = toDigits x
    firstTwo = toInt $ take 2 digits
    firstFour = toInt $ take 4 digits

isVisa :: Integer -> Bool
isVisa x = digits !! 0 == 4 && (length digits == 13 || length digits == 16 || length digits == 19) && luhn digits
    where digits = toDigits x

checkValidity checker numbers unexpected name = do
    let values = map checker numbers
    if elem unexpected values then
        do
            putStrLn $ show $ filter (\(x,y) -> x == unexpected) $ zip values numbers
            putStrLn ("Error in " ++ name)
    else
        putStrLn (name ++ " passed test")


exerciseSeven = do
    let amExNumbers = [373880337659522,377661975930391,370701162614742,349352372863923,375684547926772,349813813922412,340801991391970,347227713429919,377693123451798,378370472971119,372792467814994,374347704665671,373796125847907,373719505757470,371983315150506,372442925729802,378761088197364,375448726177957,347037371661020,342854717755257,373306288377625,342211585475577,343235419902463,376546687823888,340273064581244,342450283948119,342256954114844,349115366366777,377654754141546,378606600233009,341307706114870,343380872740677,348812969536753,378306161052167,371838447299995,373674236619392,345655746263164,348286707091050,373836774757443,346149459313811,342415344945040,379602483932501,341312452516401,343129448230451,376189975125680,343679590775703,349819269009091,374955112234599,347614146045117,370740738438046,373373375175296,349817498043535,373117442625498,371264245645604,372503512233761,349160815410628,342008489695155,379950254675316,371341176890159,378850151972538,346763626907725,347126422452404,375389351543958,343082305362141,376780417994848,378375396294720,346421566536380,373491080805385,375991363129153,349902146247439,376165651713619,379445005082623,375195359659781,340112952438960,340323978393929,347341468881859,346221984975826,343358339513713,343463751856754,371350242042570,340790570449921,372773589068289,343917107175342,343888156173233,374769396202994,371448425333707,378831860414798,377375345776612,341936721521436,344422450175139,348092939556807,342247682049634,378865138661095,378509755088006,378315008336071,343464878634934,347314787271557,345195608810646,343823260827218,375648708635522]
    let masterNumbers = [5555555555554444, 5437291847566632, 5531580388608737, 5145693648201538, 5206784260722632]
    let visaNumbers = [4111111111111111, 4271706756598344, 4485585424322309, 4716463000295458, 4929165220142104]
    let wrongAmExNumbers = [x*100000000000000 | x <- [1..99], x /= 34 && x /= 37]
    let wrongMasterNumbers = [x*100000000000000 | x <- [1..99], not ((x >= 21 && x <= 27) || (x >= 51 && x <= 55))]
    let wrongVisaNumbers = [x*100000000000000 | x <- [1..99], x /= 4 && x /= 42]

    checkValidity isAmericanExpress amExNumbers False "American Express Valid Values"
    checkValidity isMaster masterNumbers False "Mastercard Valid Values"
    checkValidity isVisa visaNumbers False "Visa Valid Values"
    checkValidity isAmericanExpress wrongAmExNumbers True "American Express Invalid Values"
    checkValidity isMaster wrongMasterNumbers True "Mastercard Invalid Values"
    checkValidity isVisa wrongVisaNumbers True "Visa Invalid Values"

-- I took the Haskell premaster with the Programming in Haskell book, so I already had the luhn function readily available.
-- Some guy put the answers on github: https://github.com/adrianwong/programming-in-haskell-2nd-edition/blob/master/ch07_solutions.hs

-- Exercise 8
-- We used the exercise of Arjan because he had the cleanest code with results we could all agree on
data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq, Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses x y | x == Matthew = y /= Matthew && y /= Carl
            | x == Peter = y == Matthew || y == Jack
            | x == Jack = not (accuses Matthew y) && not (accuses Peter y)
            | x == Arnold = accuses Matthew y /= accuses Peter y
            | x == Carl = not (accuses Arnold y)

accusers :: Boy -> [Boy]
accusers x = accusers' x boys []

-- Helper method to find accusers
accusers' :: Boy -> [Boy] -> [Boy] -> [Boy]
accusers' b (x:xs) ys | accuses x b = accusers' b xs (x:ys)
                      | otherwise = accusers' b xs ys
accusers' b [] ys = ys

-- Used source [2]
guilty, honest :: [Boy]
guilty = [ x | x <- boys, length (accusers x) == 3]
honest = [ x | x <- boys, accuses x (head guilty)]

exerciseEight = do
    putStrLn $ show $ guilty
    putStrLn $ show $ honest

-- Guilty : Jack
-- Honest: Matthew, Peter & Carl

-- Euler 9
-- We used the exercise of Constantijn because he had the most efficient code
-- Based on: https://stackoverflow.com/questions/7261667/pythagorean-triple-in-haskell-without-symmetrical-solutions/33727703#33727703
pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = [(a, b, c) | c <- [1..(div n 2)], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 && a + b + c == n]

eulerNine = do putStrLn $ show $ pythagoreanTriples 1000

-- Euler 10
-- We used the exercise of Niels because we preferred the useage of 'primesBelow' over 'takeWhile'
eulerTen = do
    putStrLn $ show $ sum (primesBelow 2000000)

-- Euler 49
-- We used the exercise of Michael because Michael was the only one who made this exercise
eulerFourtyNine = do
    putStrLn $ show $ [(x, x + 3330, x + 6660) | x <- (primesBelow 4000), elem (toDigits (x + 3330)) (permutations (toDigits x)), elem (toDigits (x + 6660)) (permutations (toDigits x)), prime (x + 3330), prime (x + 6660)]
-- This gives back two results: one tuple that is given in the problem and another tuple that is the other sequence the problem is talking about. Concatenated, this gives 296962999629.

main = do
    exerciseOne
    exerciseTwo
    exerciseThree
    exerciseFour
    exerciseFive
    exerciseSix
    exerciseSeven
    exerciseEight
    eulerNine
    eulerTen
    eulerFourtyNine
