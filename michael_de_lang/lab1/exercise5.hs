import Data.List
import System.Environment

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)

primes = sieve [2..]

sumSmallestConsecutivePrimes :: Int -> Int -> [Integer]
sumSmallestConsecutivePrimes x s | isPrime (sum consecutivePrimes) = (sum consecutivePrimes) : consecutivePrimes
                                 | otherwise = sumSmallestConsecutivePrimes x (s+1)
                                 where consecutivePrimes = (take x (drop s primes))

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

-- usage: ./exercise5 <number of primes>
-- example: ./exercise5 101
--          37447
main = do
  args <- getArgs
  let result = sumSmallestConsecutivePrimes (read (head args)) 0
  putStrLn ("result = " ++ (show $ head result))
  putStrLn ("consisting out of: " ++ (show $ tail result))


-- inspiration: https://stackoverflow.com/questions/17993074/haskell-how-to-read-in-command-line-args-as-int
-- A test would be to check sumSmallestConsecutivePrimes against known correct values. sumSmallestConsecutivePrimes 2, for example, should give 2 + 3 = 5 and sumSmallestConsecutivePrimes 3 should give 5 + 7 + 11 = 23
-- 
-- Time taken: 45m
