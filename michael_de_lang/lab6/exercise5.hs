import Lecture6 hiding (composites)
import Control.Monad

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
        minFool' newX' (n-1)
    else
        minFool' x (n-1)
    where newX = firstFool

minFool :: IO Integer
minFool = do
    x <- firstFool
    minFool' x 5
    
main = do
    x <- minFool
    print $ x
    
-- time taken 10m
{-
C:\Users\Oipo\Documents\ST2017_WG_12\michael_de_lang\lab6>ghc -O2 exercise5.hs
[2 of 2] Compiling Main             ( exercise5.hs, exercise5.o )
Linking exercise5.exe ...

C:\Users\Oipo\Documents\ST2017_WG_12\michael_de_lang\lab6>exercise5.exe
294409
-}