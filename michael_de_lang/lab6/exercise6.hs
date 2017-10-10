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
    y <- sequence $ map (\x -> primeMR 2 $ 2^x - 1) $ take 9 primes
    print $ zip y primes
   
-- time taken 6a: 30m
-- time taken 6b: 10m   
{-
oipo@sd-59673:~/uva/software_testing/STS1/michael_de_lang/lab6$ ghc -O2 exercise6.hs && ./exercise6
[2 of 2] Compiling Main             ( exercise6.hs, exercise6.o )
Linking exercise6 ...
"6a"
[False]
"6b"
[(True,2),(True,3),(True,5),(True,7),(False,11),(True,13),(True,17),(True,19),(False,23)]
-}
