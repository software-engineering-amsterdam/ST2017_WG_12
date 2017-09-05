import Data.List

reversal :: Integer -> Integer
reversal = read . reverse . show

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)

primes x = sieve [2..x]

reversablePrime :: Integer -> [Integer] -> Bool
reversablePrime n listOfPrimes = elem n listOfPrimes && elem (reversal n) listOfPrimes

main = do 
  mapM_ putStr [show x ++ ", " | x <- [1..10000], reversablePrime x (primes 10000)]
  putStrLn ""
  putStrLn "done"

-- Personally, I would test this function by (manually) calculating a known good list and compare the output of the function against that.
-- This would only be a partial specification test.
-- Time taken: 30m
