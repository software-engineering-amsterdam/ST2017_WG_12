import Lecture6 hiding (composites)
import Control.Monad
import Debug.Trace

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
        trace (show newX' ++ " - " ++ show x) $ minFool' k newX' (n-1)
    else
        trace (show newX' ++ " " ++ show x) $ minFool' k x (n-1)
    where newX = firstFool k

minFool :: Int -> IO Integer
minFool k = do
    x <- firstFool k
    minFool' k x 100
    
    
main = do
    x <- minFool 4
    print x