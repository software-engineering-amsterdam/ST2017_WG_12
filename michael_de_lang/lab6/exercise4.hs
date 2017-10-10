import Lecture6 hiding (composites)
import Control.Monad

composites :: [Integer]
composites = [x | x <- [2..], not (prime x)]

-- https://stackoverflow.com/questions/1133800/haskell-monadic-takewhile
sequenceWhile :: (Monad m) => (a -> Bool) -> [m a] -> m [a]
sequenceWhile p xs = foldr (myLiftM2 (:) []) (return []) xs
  where myLiftM2 f z m1 m2 = do
            x1 <- m1
            if p x1 then do x2 <- m2
                            return $ f x1 x2
                    else return z

printFirstFooled :: IO [a] -> IO Integer
printFirstFooled ms = do
    ys <- ms
    return $ composites !! (length ys)
    
firstFool :: Int -> IO Integer
firstFool k = printFirstFooled $ sequenceWhile (== False) $ map (primeTestsF k) composites

minFool' :: Int -> Integer -> Int -> IO Integer
minFool' k x 0 = return x;
minFool' k x n = do
    newX' <- newX
    if newX' < x then
        minFool' k newX' (n-1)
    else
        minFool' k x (n-1)
    where newX = firstFool k

minFool :: Int -> IO Integer
minFool k = do
    x <- firstFool k
    minFool' k x 100
    
    
main = do
    x <- minFool 1
    print $ "k = 1: " ++ show x
    x <- minFool 2
    print $ "k = 2: " ++ show x
    x <- minFool 3
    print $ "k = 3: " ++ show x
    x <- minFool 4
    print $ "k = 4: " ++ show x
    
-- time taken 60m
{-
oipo@sd-59673:~/uva/software_testing/STS1/michael_de_lang/lab6$ ghc -O2 exercise4.hs
[2 of 2] Compiling Main             ( exercise4.hs, exercise4.o )
Linking exercise4 ...
oipo@sd-59673:~/uva/software_testing/STS1/michael_de_lang/lab6$ ./exercise4     "k = 1: 9"
"k = 2: 9"
"k = 3: 15"
"k = 4: 15"
-}