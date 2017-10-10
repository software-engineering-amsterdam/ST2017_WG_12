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
         | y == 1 = (rem x n, x)
         | otherwise = (rem ((fst z) * (rem (snd z) n)) n, p)
         where 
              z = (e1 x (div y 2) n)
              p = (snd z) * (snd z)


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

-- TODO run profiling on actual exercise 1 and 2.
-- See exercise1.prof and exercise2.prof

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
sequenceWhile :: (Monad m) => (a -> Bool) -> [m a] -> m [a]
sequenceWhile p xs = foldr (myLiftM2 (:) []) (return []) xs
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
firstFool comp k = printFirstFooled comp $ sequenceWhile (== False) $ map (primeTestsF k) comp

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
-- Michael: 10m
-- Arjan: 5m
-- Constantijn: 15m
-- Niels: 15m 

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

exerciseFive = do
    x <- minFool carmichael 0
    print $ x
    
{-
Output when put into its own file and compiled:
C:\Users\Oipo\Documents\ST2017_WG_12\michael_de_lang\lab6>ghc -O2 exercise5.hs
[2 of 2] Compiling Main             ( exercise5.hs, exercise5.o )
Linking exercise5.exe ...

C:\Users\Oipo\Documents\ST2017_WG_12\michael_de_lang\lab6>exercise5.exe
294409
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

main = do
    print "6a"
    x <- sequence $ map (\x -> primeMR 1 x) $ take 1 carmichael
    print $ x
    
    print "6b"
    y <- sequence $ map (\x -> primeMR 2 $ 2^x - 1) $ take 9 primes
    print $ zip y primes