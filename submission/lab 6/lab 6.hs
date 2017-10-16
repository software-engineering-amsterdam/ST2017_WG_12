import Lecture6 hiding (composites)
import Control.Monad

-- exercise 1
--time taken:
-- Michael: 90m
-- Arjan: 45m
-- Constantijn: 15m
-- Niels: 20m

exM' :: Integer -> Integer -> Integer -> Integer
exM' x y n = fst (e1 x y n)

e1 :: Integer -> Integer -> Integer -> (Integer,Integer)
e1 x y n | y == 0 = (rem 1 n, 1)
         | rem y 2 == 1 = (rem ((rem x n) * (fst q)) n, x * (snd q))
         | otherwise = (rem ((fst z) * (rem (snd z) n)) n, p)
         where 
              z = (e1 x (div y 2) n)
              p = (snd z) * (snd z)
              q = (e1 x (y-1) n)

exerciseOne = do
                print "Running exM 2342 32454232 6"
                print "  Created exM:"
                print ("  " ++ show (exM' 2342 32454232 6))
                print "Origional exM:"
                print ("  " ++  show (exM 2342 32454232 6))

-- exercise 2
--time taken:
-- Michael: 5m
-- Arjan: 30m
-- Constantijn: 5m
-- Niels: 5m 

-- See ExerciseTwo - own function.txt and ExerciseTwo - origional function.txt
  -- Used values for exM:
  -- 2342 32454232 6

exerciseTwo = print $ exM 2342 32454232 6

-- exercise 3
--time taken:
-- Michael: 5m
-- Arjan: 10m
-- Constantijn: 15m
-- Niels: 15m 

-- All of us had the same implementation, we just found that using the not (prime x) version was more readable.
composites :: [Integer]
composites = [x | x <- [2..], not (prime x)]

-- exercise 4
--time taken:
-- Michael: 60m
-- Arjan: 15m
-- Constantijn: 180m
-- Niels: 45m 

-- https://stackoverflow.com/questions/1133800/haskell-monadic-takewhile
sequenceUntil :: (Monad m) => (a -> Bool) -> [m a] -> m [a]
sequenceUntil p xs = foldr (myLiftM2 (:) []) (return []) xs
  where myLiftM2 f z m1 m2 = do
            x1 <- m1
            if p x1 then do x2 <- m2
                            return $ f x1 x2
                    else return z

printFirstFooled :: [Integer] -> IO [a] -> IO Integer
printFirstFooled comp ms = do
    ys <- ms
    return $ comp !! (length ys)
    
firstFool :: [Integer] -> Int -> IO Integer
firstFool comp k = printFirstFooled comp $ sequenceUntil (== False) $ map (primeTestsF k) comp

minFool' :: [Integer] -> Int -> Integer -> Int -> IO Integer
minFool' comp k x 0 = return x;
minFool' comp k x n = do
    newX' <- newX
    if newX' < x then
        minFool' comp k newX' (n-1)
    else
        minFool' comp k x (n-1)
    where newX = firstFool comp k

minFool :: [Integer] -> Int -> IO Integer
minFool comp k = do
    x <- firstFool comp k
    minFool' comp k x 100
    
    
exerciseFour = do
    x <- minFool composites 0
    print $ "k = 0: " ++ show x
    x <- minFool composites 1
    print $ "k = 1: " ++ show x
    x <- minFool composites 2
    print $ "k = 2: " ++ show x
    x <- minFool composites 3
    print $ "k = 3: " ++ show x
    x <- minFool composites 4
    print $ "k = 4: " ++ show x
    
{-
primeTestsF is a function that uses probability to check whether a number is a prime or not. Running this test 100 times generates a certain distribution
based on the given k. Increasing the k to 4, for example, leaves it incredibly unlikely for primeTestsF to mark 9 as a prime, but it still happens every 
once in a blue moon.

Output when put into its own file and compiled:
oipo@sd-59673:~/uva/software_testing/STS1/michael_de_lang/lab6$ ghc -O2 exercise4.hs
[2 of 2] Compiling Main             ( exercise4.hs, exercise4.o )
Linking exercise4 ...
oipo@sd-59673:~/uva/software_testing/STS1/michael_de_lang/lab6$ ./exercise4
"k = 0: 4"
"k = 1: 9"
"k = 2: 9"
"k = 3: 15"
"k = 4: 15"
-}

-- exercise 5
--time taken:
-- Michael: 30m
-- Arjan: 25m
-- Constantijn: 35m
-- Niels: 35m 

-- The chance of a carmichael number being marked as prime is bigger than 
-- a random composite number. We compared the chance of the first carmichael number passing as prime to 
-- the chance of the composite number 15. On average, carmichael passes for a prime 94 times and the number 15 passes 19 times.

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

exerciseFive = do
    xs <- sequence $ map (\x -> primeTestsF 1 $ carmichael !! 0) [1..100]
    ys <- sequence $ map (\x -> primeTestsF 1 $ composites !! 7) [1..100]
    print $ length $ filter (== True) xs
    print $ length $ filter (== True) ys

    
{-
Output when put into its own file and compiled:
*Main> main
94
19
-}

-- exercise 6
--time taken 6a:
-- Michael: 30m
-- Arjan: 0m
-- Constantijn: 0m
-- Niels: 10m

--time taken 6b:
-- Michael: 10m
-- Arjan: 10m
-- Constantijn: 10m
-- Niels: 10m

-- 6a:
-- primeMR is significantly more accurate than primeTestsF, though still not 100%.

-- 6b:
-- For the first 9 primes we can say that the primeMR function correctly identifies mersenne primes.
-- After the 9th prime, computation time becomes a problem.

exerciseSix = do
    print "6a"
    xs <- sequence $ map (\x -> primeTestsF 1 $ carmichael !! 0) [1..100]
    ys <- sequence $ map (\x -> primeMR 1 $ carmichael !! 0) [1..100]
    print $ length $ filter (== True) xs
    print $ length $ filter (== True) ys
    
    print "6b"
    y <- sequence $ map (\x -> primeMR 2 $ 2^x - 1) $ take 9 primes
    print $ zip y primes

{-
*Main> exerciseSix
"6a"
94
6
"6b"
[(True,2),(True,3),(True,5),(True,7),(False,11),(True,13),(True,17),(True,19),(False,23)]
-}
    
    
-- Exercise 7 Bonus
-- Source of the toBin-function
-- https://stackoverflow.com/questions/9166148/how-to-implement-decimal-to-binary-function-in-haskell
toBin 0 = [0]
toBin n = reverse (helper n)

helper 0 = []
helper n | n `mod` 2 == 1 = 1 : helper (n `div` 2)
         | n `mod` 2 == 0 = 0 : helper (n `div` 2)

nthPrime :: Int -> Integer
nthPrime x = primes !! x         

bitLength :: Integer -> Int
bitLength x = length (toBin x)
         
primesWithBitLength :: Int -> [Integer]
primesWithBitLength n = filter (\x -> bitLength x == n) (takeWhile (\x -> bitLength x < (n + 1)) primes)
            
-- Inspiration for generating all possible tuples
-- https://codereview.stackexchange.com/questions/152190/producing-all-possible-combinations-of-a-listhttps://codereview.stackexchange.com/questions/152190/producing-all-possible-combinations-of-a-list
createGroups :: [a] -> [(a, a)]
createGroups [] = []
createGroups (x:xs) = map ((,) x) xs ++ createGroups xs  

primePairs :: Int -> [(Integer, Integer)]
primePairs x = createGroups (primesWithBitLength x)

encodeDecodeCheck :: (Integer, Integer) -> Integer -> Bool
encodeDecodeCheck (x,y) message = message == rsaDecode private (rsaEncode public message)
                        where 
                        public = rsaPublic x y
                        private = rsaPrivate x y

exerciseSeven = do
                let x = head (primePairs 5)
                print $ encodeDecodeCheck x 10
                        
{-
proof!
Lab6> let x = head (primePairs 15)
*Lab6> x
(16411,16417)
*Lab6> encodeDecodeCheck x 10
True
*Lab6> encodeDecodeCheck x (2^24)
True
*Lab6> encodeDecodeCheck x (2^25)
True
*Lab6> encodeDecodeCheck x (2^26)
True
*Lab6> encodeDecodeCheck x (2^27)
True
*Lab6>
-}
        
    
    