import Lecture6 hiding (composites)
import Control.Monad

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]
    
main = do
    x <- sequence $ map (\x -> primeMR 1 x) $ take 5 carmichael
    print $ x
    
    
{-
C:\Users\Oipo\Documents\ST2017_WG_12\michael_de_lang\lab6>ghc -O2 exercise5.hs
[2 of 2] Compiling Main             ( exercise5.hs, exercise5.o )
Linking exercise5.exe ...

C:\Users\Oipo\Documents\ST2017_WG_12\michael_de_lang\lab6>exercise5.exe
294409
-}