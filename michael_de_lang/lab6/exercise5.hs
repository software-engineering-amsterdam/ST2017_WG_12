import Lecture6 hiding (composites)
import Control.Monad
import Debug.Trace

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]
      
      
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
    return $ carmichael !! (length ys)
    
firstFool :: IO Integer
firstFool = printFirstFooled $ sequenceWhile (== False) $ map (primeTestF) carmichael
    
minFool' :: Integer -> Int -> IO Integer
minFool' x 0 = return x;
minFool' x n = do
    newX' <- newX
    if newX' < x then
        trace (show newX' ++ " " ++ show x ++ " " ++ show n) $ minFool' newX' (n-1)
    else
        trace (show newX' ++ " " ++ show x ++ " " ++ show n) $ minFool' x (n-1)
    where newX = firstFool

minFool :: IO Integer
minFool = do
    x <- firstFool
    minFool' x 100
    
main = do
    x <- minFool
    print $ x
    
    
{-
oipo@sd-59673:~/uva/software_testing/STS1/michael_de_lang/lab6$ ghc -O2 exercise4.hs
[2 of 2] Compiling Main             ( exercise4.hs, exercise4.o )
Linking exercise4 ...
oipo@sd-59673:~/uva/software_testing/STS1/michael_de_lang/lab6$ ./exercise4     "k = 1: 9"
"k = 2: 9"
"k = 3: 15"
"k = 4: 15"
-}