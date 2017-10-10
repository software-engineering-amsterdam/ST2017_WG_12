import Lecture6 hiding (composites)
import Control.Monad

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]
    
main = do
    print "6a"
    x <- sequence $ map (\x -> primeMR 1 x) $ take 1 carmichael
    print $ x
    
    print "6b"
    y <- sequence $ map (\x -> primeMR 1 $ 2^x - 1) $ take 25 primes
    print y
   
-- time taken 6a: 30m
-- time taken 6b: 10m   
{-
[False]
-}