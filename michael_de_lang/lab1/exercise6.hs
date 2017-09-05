import Data.List

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)

primes = sieve [2..]

multiply :: [Integer] -> Integer
multiply [] = 1
multiply (x:xs) = x * multiply xs

conjecture :: Int -> Integer 
conjecture x = 1 + multiply [x' | x' <- take x primes]

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

counterExample :: Int -> Integer
counterExample x | isPrime (conjecture x) = counterExample (x+1)
                 | otherwise = conjecture x

main = do
  putStrLn ("smallest number refuting conjecture = " ++ (show $ counterExample 0))

-- Time taken: 20m
